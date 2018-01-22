open BsReactNative;

type screenPoint = {
  mutable x: float,
  mutable y: float
};

type movePositions =
  | Corner(float, float)
  | TopBottom(float)
  | LeftRight(float);

type childProps = {mutable imageSize: float};

let component = ReasonReact.statelessComponent("Motion");

let windowDimensions = Dimensions.get(`window);

let motionProps = {imageSize: 0.};

let setMotionProps = (~imageSize) =>
  motionProps.imageSize = float_of_int(imageSize);

let pan = Animated.ValueXY.create(~x=0., ~y=0.);

let resetPan = (_) => Animated.ValueXY.extractOffset(pan);

let handleAutomatedMove = (~x=0., ~y=0., ()) =>
  Animated.TimingXY.animate(
    ~value=pan,
    ~toValue=`raw({"x": x, "y": y}),
    ~duration=300.,
    ()
  )
  |> (
    animated =>
      Animated.CompositeAnimation.start(animated, ~callback=resetPan, ())
  );

let childCoordinates = {x: 0., y: 0.};

let movePosition = p =>
  switch p {
  | Corner(x, y) => handleAutomatedMove(~x, ~y, ())
  | TopBottom(y) => handleAutomatedMove(~y, ())
  | LeftRight(x) => handleAutomatedMove(~x, ())
  };

let handleRelease = (_e, _g) => {
  let {imageSize} = motionProps;
  let imageRight = imageSize +. childCoordinates.x;
  let imageBottom = imageSize +. childCoordinates.y;
  let windowWidth = float_of_int(windowDimensions##width);
  let windowHeight = float_of_int(windowDimensions##height);
  let dimTuple = (
    childCoordinates.x,
    childCoordinates.y,
    imageRight,
    imageBottom
  );
  resetPan();
  switch dimTuple {
  | (_, t, r, _) when t < 0. && r > windowWidth =>
    movePosition(Corner(windowWidth -. r, t *. (-1.)))
  | (l, t, _, _) when l < 0. && t < 0. =>
    movePosition(Corner(l *. (-1.), t *. (-1.)))
  | (l, _, _, b) when l < 0. && b > windowHeight =>
    movePosition(Corner(l *. (-1.), windowHeight -. b))
  | (_, _, r, b) when b > windowHeight && r > windowWidth =>
    movePosition(Corner(windowWidth -. r, windowHeight -. b))
  | (_, t, _, _) when t < 0. => movePosition(TopBottom(t *. (-1.)))
  | (l, _, _, _) when l < 0. => movePosition(LeftRight(l *. (-1.)))
  | (_, _, _, b) when b > windowHeight =>
    movePosition(TopBottom(windowHeight -. b))
  | (_, _, r, _) when r > windowWidth =>
    movePosition(LeftRight(windowWidth -. r))
  | (_, _, _, _) => resetPan()
  };
};

let panResponder =
  PanResponder.create(
    ~onStartShouldSetPanResponder=PanResponder.callback((_e, _g) => true),
    ~onPanResponderMove=`update([`XY(pan)]),
    ~onPanResponderRelease=PanResponder.callback(handleRelease),
    ()
  );

let panListener = r => {
  childCoordinates.x = r##x;
  childCoordinates.y = r##y;
};

let panListener = Animated.ValueXY.addListener(pan, panListener);

let styles =
  StyleSheet.create(
    Style.(
      {
        "container":
          style([
            Transform.makeAnimated(
              ~translateX=Animated.ValueXY.getX(pan),
              ~translateY=Animated.ValueXY.getY(pan),
              ()
            )
          ])
      }
    )
  );

let handlers = PanResponder.panHandlers(panResponder);

let make = (~render, ~imageSize, _children) => {
  ...component,
  didMount: _self => {
    setMotionProps(~imageSize) |> ignore;
    ReasonReact.NoUpdate;
  },
  willUnmount: _self => Animated.ValueXY.removeAllListeners(pan),
  render: _self =>
    <View responderHandlers=handlers>
      <Animated.View style=styles##container> (render()) </Animated.View>
    </View>
};