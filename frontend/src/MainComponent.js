export function _getSVGElementById(nothing) {
    return function(just) {
        return function(svgElementId) {
            return function(elementId) {
                return function() {
                    var element = document.getElementById(svgElementId)?.contentDocument?.getElementById?.(elementId);
                    if (element == null) {
                        return nothing;
                    } else {
                        return just(element);
                    }
                }
            }
        }
    }
};
