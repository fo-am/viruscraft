/**
 * Provides requestAnimationFrame in a cross browser way.
 */
 // var requestAnimFrame = (function() {
 //    return window.requestAnimationFrame ||
 //    window.webkitRequestAnimationFrame ||
 //    window.mozRequestAnimationFrame ||
 //    window.oRequestAnimationFrame ||
 //    window.msRequestAnimationFrame ||
 //    function(/* function FrameRequestCallback */ callback, /* DOMElement Element */ element) {
 //    window.setTimeout(callback, 1000/60);    };
 //    })();

function do_confirm() {
    return confirm("Are you sure?");
}
