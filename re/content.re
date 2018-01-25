open BsReactNative;

let component = ReasonReact.statelessComponent("Content");

let styles =
  StyleSheet.create(
    Style.({"image": style([height(Pt(300.)), width(Pt(300.))])})
  );

let make = _children => {
  ...component,
  render: _self =>
    <Motion
      imageSize={width: 300., height: 300.}
      containerSize={width: 375., height: 400.}
      render=(
        () =>
          <Image
            style=styles##image
            source=(
              URI(Image.imageURISource(~uri="https://picsum.photos/300", ()))
            )
          />
      )
    />
};