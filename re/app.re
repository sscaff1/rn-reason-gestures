let component = ReasonReact.statelessComponent("App");

let make = _children => {...component, render: _self => <Content />};

let default = ReasonReact.wrapReasonForJs(~component, (_) => make([||]));