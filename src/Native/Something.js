// BEGIN ElmNativeWrapper boilerplate
(function ElmNativeWrapper(jsName, moduleName, actualNativeModule) {

  var mod = window[jsName] = {
    initialised: false,
    appName: null,
    mungedName: null,
    init: function(appName) {
      mod.appName = appName;
      mod.mungedName = mungeApp(appName) + "$" + mungeModule(moduleName);
      window[mod.mungedName] = actualNativeModule();
      mod.initialised = true;
    }
  };

  setTimeout(function() {
    if (!mod.initialised) {
      console.error(
        "Native module `" + jsName + "` was not initialised\n" +
        "You must call `" + jsName + ".init(appName)` to make this work"
      );
    }
  }, 1000);

  function mungeApp(x) {
    // TODO: full munging logic
    var bits = x.split("/");
    return "_" + bits[0] + "$" + bits[1].replace("-", "_");
  }

  function mungeModule(x) {
    // TODO: full munging logic
    return x.replace(".", "_");
  }

  // Make Webpack HMR work
  if (module.hot) {
    module.hot.dispose(function(data) { data.appName = mod.appName; });
    if (module.hot.data && module.hot.data.appName) {
      mod.init(module.hot.data.appName);
    }
  }

})(
// END ElmNativeWrapper boilerplate
  "ElmSomething",
  "Native.Something",
  function actualNativeModule() {
    return {
      whatever: "yes",
    };
  }
);
