import http from 'http';

export function runLaff(laff) {
  return function() {
    function finalContinuation(a) {};
    try {
      laff(finalContinuation);
    } catch (error) {
      console.log("Error in laff", error);
    }
  }
}

export function _map(f) {
  return function(laff){
    return function(continuation){
      function fContinuation(a){
        let b = f(a);
        continuation(b);
      }
      laff(fContinuation);
    }
  }
}

export function _pure(a) {
  return function(continuation){
    continuation(a);
  }
}

export function _bind(laffA) {
  return function(fLaffB){
    return function(continuation){
      // NOTE: By calling via setTimeout, we break direct recursive loops
      // by delegating the call to the runtime scheduler.
      // Without setTimeout, recursive Laff functions can blow the stack.
      setTimeout(function(){
          laffA(function(a){fLaffB(a)(continuation)})
      }, 0);
    }
  }
}

export function _liftEffect(eff) {
  return function(continuation) {
    continuation(eff());
  }
}

export function delay(millis) {
  return function(continuation) {
    setTimeout(function(){ continuation() }, millis);
  }
}

// https://stackoverflow.com/a/31090240
var isBrowser = new Function("try {return this===window;}catch(e){ return false;}");

export function httpRequest(url) {
  return function(continuation) {
    if (isBrowser()){
      const req = new XMLHttpRequest();
      req.addEventListener("load", function(){continuation(this.responseText)});
      req.open("GET", url);
      req.send();
    } else {
      http.get({
        hostname: url,
      }, function(res){
        if (res.statusCode !== 200) {
          console.error(`Request Failed with status code: ${res.statusCode}`);
        }
        res.on('data', function(chunk){continuation(chunk.toString())});
      });
    }
  }
}
