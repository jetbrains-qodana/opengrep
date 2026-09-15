function handleA() {
  var res = new Response();
  // ruleid: match-simple-metavar-type-js
  res.send("hello");
}

function handleB() {
  var res = new Headers();
  // ok: type-mismatch
  res.send("hello");
}
