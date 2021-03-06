open BsReactNative;

type screenPoint = {
  mutable x: float,
  mutable y: float
};

type movePositions =
  | Corner(float, float)
  | TopBottom(float)
  | LeftRight(float);

type dimensions = {
  width: float,
  height: float
};

type childProps = {
  mutable image: dimensions,
  mutable container: dimensions,
  mutable initialDistance: float,
  mutable currentScaleValue: float
};

let component = ReasonReact.statelessComponent("Motion");

let windowDimensions = Dimensions.get(`window);

let motionProps = {
  image: {
    width: 0.,
    height: 0.
  },
  container: {
    width: 0.,
    height: 0.
  },
  initialDistance: 0.,
  currentScaleValue: 1.
};

let pan = Animated.ValueXY.create(~x=0., ~y=0.);

let scale = Animated.Value.create(1.);

let childCoordinates = {x: 0., y: 0.};

let childScale = ref(1.);

let resetPan = (_) => Animated.ValueXY.extractOffset(pan);

let handleAutomatedMove = (~x=0., ~y=0., ~duration=300., ()) =>
  Animated.TimingXY.animate(
    ~value=pan,
    ~toValue=`raw({"x": x, "y": y}),
    ~duration,
    ()
  )
  |> (
    animated =>
      Animated.CompositeAnimation.start(animated, ~callback=resetPan, ())
  );

let setMotionProps = (~imageSize, ~containerSize, ~initialPoint) => {
  motionProps.image = imageSize;
  motionProps.container = (
    switch containerSize {
    | None => {
        width: float_of_int(windowDimensions##width),
        height: float_of_int(windowDimensions##height)
      }
    | Some(s) => s
    }
  );
  let {x, y} =
    switch initialPoint {
    | None => {
        x: (motionProps.container.width -. motionProps.image.width) /. 2.,
        y: (motionProps.container.height -. motionProps.image.height) /. 2.
      }
    | Some(i) => i
    };
  handleAutomatedMove(~x, ~y, ~duration=0., ());
};

let movePosition =
  fun
  | Corner(x, y) => handleAutomatedMove(~x, ~y, ())
  | TopBottom(y) => handleAutomatedMove(~y, ())
  | LeftRight(x) => handleAutomatedMove(~x, ());

let handlePanStart = (_e, g: PanResponder.gestureState) =>
  g.numberActiveTouches == 1;

let adjustChildPosition = () => {
  let {image, container, currentScaleValue} = motionProps;
  let imageWidth = image.width *. currentScaleValue;
  let imageHeight = image.height *. currentScaleValue;
  let imageLeft =
    childCoordinates.x +. (1. -. currentScaleValue) *. image.width /. 2.;
  let imageTop =
    childCoordinates.y +. (1. -. currentScaleValue) *. image.height /. 2.;
  let imageRight = imageWidth +. imageLeft;
  let imageBottom = imageHeight +. imageTop;
  let dimTuple = (imageLeft, imageTop, imageRight, imageBottom);
  resetPan();
  switch dimTuple {
  | (_, t, r, _) when t < 0. && r > container.width =>
    movePosition(Corner(container.width -. r, t *. (-1.)))
  | (l, t, _, _) when l < 0. && t < 0. =>
    movePosition(Corner(l *. (-1.), t *. (-1.)))
  | (l, _, _, b) when l < 0. && b > container.height =>
    movePosition(Corner(l *. (-1.), container.height -. b))
  | (_, _, r, b) when b > container.height && r > container.width =>
    movePosition(Corner(container.width -. r, container.height -. b))
  | (_, t, _, _) when t < 0. => movePosition(TopBottom(t *. (-1.)))
  | (l, _, _, _) when l < 0. => movePosition(LeftRight(l *. (-1.)))
  | (_, _, _, b) when b > container.height =>
    movePosition(TopBottom(container.height -. b))
  | (_, _, r, _) when r > container.width =>
    movePosition(LeftRight(container.width -. r))
  | (_, _, _, _) => resetPan()
  };
};

let handleRelease = (_e, _g) => adjustChildPosition();

let panResponder =
  PanResponder.create(
    ~onStartShouldSetPanResponder=PanResponder.callback(handlePanStart),
    ~onPanResponderMove=`update([`XY(pan)]),
    ~onPanResponderRelease=PanResponder.callback(handleRelease),
    ()
  );

let handleScaleStart = (e, g: PanResponder.gestureState) =>
  switch g.numberActiveTouches {
  | touches when touches > 1 =>
    motionProps.initialDistance =
      e |> RNEvent.NativeEvent.touches |> Utilities.distance;
    true;
  | _ => false
  };

let handleScaleMove = (e, _g) =>
  Utilities.distance(e |> RNEvent.NativeEvent.touches)
  /. motionProps.initialDistance
  *. motionProps.currentScaleValue
  |> Animated.Value.setValue(scale);

let handleScaleRelease = (_e, _g) =>
  motionProps.currentScaleValue = childScale^;

let scaleResponder =
  PanResponder.create(
    ~onStartShouldSetPanResponder=PanResponder.callback(handleScaleStart),
    ~onPanResponderMove=`callback(PanResponder.callback(handleScaleMove)),
    ~onPanResponderRelease=PanResponder.callback(handleScaleRelease),
    ()
  );

let panListener = r => {
  childCoordinates.x = r##x;
  childCoordinates.y = r##y;
};

let scaleListener = s => childScale := s##value;

let panListener = Animated.ValueXY.addListener(pan, panListener);

let scaleListener = Animated.Value.addListener(scale, scaleListener);

let styles =
  StyleSheet.create(
    Style.(
      {
        "container":
          style([
            Transform.makeAnimated(
              ~translateX=Animated.ValueXY.getX(pan),
              ~translateY=Animated.ValueXY.getY(pan),
              ~scaleY=scale,
              ~scaleX=scale,
              ()
            )
          ])
      }
    )
  );

let handlers = PanResponder.panHandlers(panResponder);

let scaleHandlers = PanResponder.panHandlers(scaleResponder);

let make = (~render, ~imageSize, ~containerSize=?, ~initialPoint=?, _children) => {
  ...component,
  didMount: _self => {
    setMotionProps(~imageSize, ~containerSize, ~initialPoint);
    ReasonReact.NoUpdate;
  },
  willUnmount: _self => Animated.ValueXY.removeAllListeners(pan),
  render: _self =>
    <View responderHandlers=handlers>
      <View responderHandlers=scaleHandlers>
        <Animated.View style=styles##container> (render()) </Animated.View>
      </View>
    </View>
};