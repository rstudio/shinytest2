// The content of this file gets injected into the Shiny application that is
// in the iframe. This is the application for which interactions are being
// recorded.

window.shinyRecorder = (function() {
    var shinyrecorder = {
        sendWindowSize: null,
        initialized: false,
        token: null        // Gets set by parent frame
    };

    // Store previous values for each input. Use JSON so that we can compare
    // non-primitive objects like arrays.
    var previousInputValues = {};

    // Some inputs are changed from the server (via updateTextInput and
    // similar), but we don't want to record these inputs. This keeps track of
    // inputs that were updated this way.
    var updatedInputs = {};

    // When the client receives output values from the server, it's possible
    // for an html output to contain a Shiny input, with some default value.
    // In that case, we don't want to record the input event, because it's
    // automatic. When this happens, it will trigger a shiny:inputchanged
    // event on the same tick.
    var waitingForInputChange = false;

    $(document).on("shiny:inputchanged", function(event) {
        // If the value has been set via a shiny:updateInput event, we want to
        // ignore it. To do this, we'll add it to the previous values list.
        // For some inputs (like sliders), when a value is updated, it can
        // result in  shiny:inputchanged getting triggered more than once, so
        // we need to make sure that we ignore it this time and future times.
        if (updatedInputs[event.name]) {
            previousInputValues[event.name] = JSON.stringify(event.value);
            delete updatedInputs[event.name];
            return;
        }

        // If this input change was triggered by an html output, don't record
        // it.
        if (waitingForInputChange) {
            previousInputValues[event.name] = JSON.stringify(event.value);
            delete updatedInputs[event.name];
            return;
        }

        // Check if value has changed from last time.
        if (event.priority !== "event") {
            var valueJSON = JSON.stringify(event.value);
            if (valueJSON === previousInputValues[event.name])
                return;
            previousInputValues[event.name] = valueJSON;
        }

        var hasBinding = !!event.binding;
        sendInputEventToParent(event.inputType, event.name, event.value, hasBinding, event.priority);
    });

    $(document).on("shiny:filedownload", function(event) {
        sendFileDownloadEventToParent(event.name);
    });

    $(document).on("shiny:value", function(event) {
        // For now, we only care _that_ outputs have changed, but not what
        // they are.
        sendOutputEventToParentDebounced();

        // This is used to detect if any output updates trigger an input
        // change.
        waitingForInputChange = true;
        setTimeout(function() { waitingForInputChange = false; }, 0);
    });

    // Register input updates here and ignore them in the shiny:inputchanged
    // listener.
    $(document).on("shiny:updateinput", function(event) {
        var inputId = event.binding.getId(event.target);
        updatedInputs[inputId] = true;
        // Schedule this updated input to be cleared at the end of this tick.
        // This is useful in the case where an input is updated with an empty
        // value -- for example, if a selectInput is updated with a number of
        // selections and a value of character(0), then it will not be removed
        // from the updatedInputs list via the other code paths. (Note that it
        // is possible in principle for other functions to be scheduled to
        // occur afterward, but on the same tick, but in practice this
        // shouldn't occur.)
        setTimeout(function() { delete updatedInputs[inputId]; }, 0);
    });

    $(document).on("click", ".shiny-bound-output", function(e) {
        if (!(e.ctrlKey || e.metaKey))
        return;

        // Ctrl-click or Cmd-click (Mac) to record an output value
        var $elOutput = $(e.target).closest(".shiny-bound-output");
        if ($elOutput.length > 0) sendOutputSnapshotToParent($elOutput[0].id);

        // Ctrl-click or Cmd-click (Mac) to record an input value
        var $elInput = $(e.target).closest(".shiny-bound-input");
        if ($elInput.length > 0) sendInputSnapshotToParent($elInput[0].id);
    });

    $(document).keydown(function(e) {
        if (!(e.ctrlKey || e.metaKey)) return;
        if (!e.shiftKey) return;

        // Trigger a snapshot on Ctrl-shift-S or Cmd-shift-S (Mac)
        if (e.which === 83) sendSreenshotSnapshotToParent();
        // Trigger a snapshot on Ctrl-shift-V or Cmd-shift-V (Mac)
        if (e.which === 86) sendValuesSnapshotToParent();
        // Trigger a wait for idle on Ctrl-shift-I or Cmd-shift-I (Mac)
        if (e.which === 73) sendWaitForIdleToParent();
    });

    function debounce(f, delay) {
        var timer = null;
        return function() {
            var context = this;
            var args = arguments;
            clearTimeout(timer);
            timer = setTimeout(function () {
                f.apply(context, args);
            }, delay);
        };
    }

    function sendMessageToParent(obj) {
      obj.token = shinyrecorder.token;
      window.parent.postMessage(obj, "*");
    }

    function sendInputEventToParent(inputType, name, value, hasBinding, priority) {
        sendMessageToParent({
            type: "inputEvent",
            inputType: inputType,
            name: name,
            value: value,
            hasBinding: hasBinding,
            priority: priority
        });
    }

    function sendFileDownloadEventToParent(name, url) {
        sendMessageToParent({
            type: "expectDownload",
            name: name
        });
    }

    function sendOutputEventToParent() {
        sendMessageToParent({
            type: "outputEvent"
        });
    }

    function sendWindowSizeToParent() {
        sendMessageToParent({
            type: "setWindowSize",
            width: window.innerWidth,
            height: window.innerHeight
        });
    }

    // will be called whenever window size changes
    var sendWindowSizeToParentDebounced = debounce(sendWindowSizeToParent, 250)
    window.addEventListener('resize', function() {
        // viewport and full window dimensions will change
        sendWindowSizeToParentDebounced();
    });
    shinyrecorder.sendWindowSize = sendWindowSizeToParentDebounced;

    // If multiple outputs are updated in a single reactive flush, the JS
    // output events will all happen in a single tick. Debouncing for one tick
    // will collapse them into a single call to sendOutputEventToParent().
    var sendOutputEventToParentDebounced = debounce(sendOutputEventToParent, 10);

    function sendSreenshotSnapshotToParent() {
        sendMessageToParent({
            type: "expectScreenshot"
        });
    }

    function sendInputSnapshotToParent(name) {
        sendMessageToParent({
            type: "expectValues",
            key: "input",
            value: name
        });
    }
    function sendOutputSnapshotToParent(name) {
        sendMessageToParent({
            type: "expectValues",
            key: "output",
            value: name
        });
    }
    function sendValuesSnapshotToParent(name) {
        sendMessageToParent({
            type: "expectValues"
        });
    }
    function sendWaitForIdleToParent(name) {
        sendMessageToParent({
            type: "waitForIdle"
        });
    }


    // ------------------------------------------------------------------------
    // Initialization
    // ------------------------------------------------------------------------
    function initialize() {
        if (shinyrecorder.initialized)
            return;

        // Save initial values so we can check for changes.
        for (var name in Shiny.shinyapp.$inputValues) {
            if (Shiny.shinyapp.$inputValues.hasOwnProperty(name)) {
                previousInputValues[name] = JSON.stringify(Shiny.shinyapp.$inputValues[name]);
            }
        }

        shinyrecorder.initialized = true;
    }
    if (Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
        initialize();
    } else {
        $(document).on("shiny:connected", initialize);
    }


    return shinyrecorder;
})();
