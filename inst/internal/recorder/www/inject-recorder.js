// This file is loaded by the recorder app.

// TODO-barret; Is it `opt` or `cmd` for mac?
// Todo-barret-test:
// * capture input
// * capture output
// * capture all values (via button)
// * capture all values (via keyboard)
// * capture screenshot (via button)
// * capture screenshot (via keyboard)
// * file download
// * update input value via `updateSliderValue()`?
// * click on input button


window.recorder = (function() {
    var recorder = {
        token: randomId(),
        testEvents: []
    };


    // Code injection
    $(document).ready(function() {

        var status = {
            frameReady: false,
            recorderCodeReady: false,
            codeHasBeenInjected: false
        };

        function evalCodeInFrame(code) {
            var message = {
                token: "abcdef",
                code: code
            };
            $('#app-iframe')[0].contentWindow.postMessage(message, "*");
        }


        // Check that the frame is ready with its Shiny app
        var frameReadyChecker = window.setInterval(function() {
            if (status.frameReady) {
                injectRecorderJS();
                clearTimeout(frameReadyChecker);
                return;
            }


            // Find out when iframe app is ready - this tells it to send back
            // a message indicating that it's ready.
            evalCodeInFrame(
                // "if (Shiny && Shiny.shinyapp && Shiny.shinyapp.config) {" +
                "if (window.$) {" +
                    "var message = {" +
                        "token: '" + recorder.token + "', " +
                        "type: 'frameReady'" +
                    "};\n" +
                    "parent.postMessage(message, '*');" +
                "}"
            );
        }, 100);

        var recorderJS;
        Shiny.addCustomMessageHandler("recorder_js", function(message) {
            status.recorderCodeReady = true;
            recorderJS = message;
            injectRecorderJS();
        });

        // Inject recorder code into iframe, but only if hasn't already been done.
        function injectRecorderJS() {
            if (!status.codeHasBeenInjected &&
                status.frameReady &&
                status.recorderCodeReady)
            {
                evalCodeInFrame(recorderJS);
                evalCodeInFrame("window.shinyRecorder.token = '" + recorder.token + "';");
                evalCodeInFrame("window.shinyRecorder.sendWindowSize();");
                status.codeHasBeenInjected = true;
            }
        }


        function triggerTestEvent(obj) {
            if (!obj.token) obj.token = recorder.token;
            obj.time = Date.now();
            recorder.testEvents.push(obj);
            // Send updated values to server
            Shiny.onInputChange("testevents:shinytest2.testevents", recorder.testEvents);
        }


        // Set up message receiver. Code is evaluated with `status` as the
        // context, so that the value can be modified in the right place.
        window.addEventListener("message", function(e) {
            var message = e.data;
            if (message.token !== recorder.token)
                return;

            function addTestEvent() {
                triggerTestEvent(message);
            }

            switch (message.type) {
                case 'frameReady':
                    status.frameReady = true;
                    message.type = "initialize";
                    addTestEvent();
                    break;
                case 'inputEvent':
                    // Filter out clientdata items
                    if (message.name.indexOf(".clientdata") === 0)
                        return;
                case 'outputEvent':
                case 'expectValues':
                case 'expectScreenshot':
                case 'expectDownload':
                case 'setWindowSize':
                case 'waitForIdle':
                    addTestEvent();
                    break;
                default:
                    console.error("Unknown message type:", message);
            }

            // console.log("message code: ", message.code);
            // (function() { eval(message.code); }).call(status);
        });

        // Generate snapshot via keypress within parent context as well
        $(document).keydown(function(e) {
            if (!(e.ctrlKey || e.metaKey)) return;
            if (!e.shiftKey) return;
            // Trigger a snapshot on Ctrl-shift-S or Cmd-shift-S (Mac)
            if (e.which === 83) triggerTestEvent({type: "expectScreenshot"});
            // Trigger a snapshot on Ctrl-shift-V or Cmd-shift-V (Mac)
            if (e.which === 86) triggerTestEvent({type: "expectValues"});
            // Trigger a snapshot on Ctrl-shift-I or Cmd-shift-I (Mac)
            if (e.which === 73) triggerTestEvent({type: "waitForIdle"});
        });

        $(document).on("shiny:inputchanged", function(event) {
            console.log("shiny:inputchanged", "Event:", event);
            if (event.name === "values") triggerTestEvent({type: "expectValues"});
            if (event.name === "screenshot") triggerTestEvent({type: "expectScreenshot"});
        });
    });


    // ------------------------------------------------------------------------
    // Utility functions
    // ------------------------------------------------------------------------

    function randomId() {
        return Math.floor(0x100000000 + (Math.random() * 0xF00000000)).toString(16);
    }

    return recorder;
})();
