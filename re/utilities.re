let pow2abs = (a, b) => (a -. b) ** 2.;

let distance = touches => {
  let [|a, b|] = touches;
  sqrt(pow2abs(a##pageX, b##pageX) +. pow2abs(b##pageY, b##pageY));
};