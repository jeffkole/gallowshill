Elm.Native.CurrentTime = {};
Elm.Native.CurrentTime.make = function (localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.CurrentTime = localRuntime.Native.CurrentTime || {};

  if (localRuntime.Native.CurrentTime.values) {
    return localRuntime.Native.CurrentTime.values;
  }

  return localRuntime.Native.CurrentTime.values = {
    now: Date.now()
  };
};
