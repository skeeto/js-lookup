;;; js-lookup-database.el --- URL database for js-lookup

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; The s-expression below maps JavaScript concepts to URLs on the
;; Mozilla Developer Network (MDN) wiki.

;;; Code:

(require 'js-lookup)

(js-lookup/root "https://developer.mozilla.org/en-US/docs/"
  (js-lookup/root "JavaScript/Reference/"

    ;; Global Objects
    (js-lookup/root "Global_Objects/"
      (js-lookup/category Array
        concat constructor every filter forEach indexOf join lastIndexOf length
        map pop push reduce reduceRight reverse shift slice some sort splice
        toString unshift)
      (js-lookup/category Boolean
        constructor toString valueOf)
      (js-lookup/category Date
        constructor getDate getDay getFullYear getHours getMilliseconds
        getMinutes getMonth getSeconds getTime getTimezoneOffset getUTCDate
        getUTCDay getUTCFullYear getUTCHours getUTCMilliseconds getUTCMinutes
        getUTCMonth getUTCSeconds getYear setDate setFullYear setHours
        setMilliseconds setMinutes setMonth setSeconds setTime setUTCDate
        setUTCFullYear setUTCHours setUTCMilliseconds setUTCMinutes setUTCMonth
        setUTCSeconds setYear toDateString toGMTString toISOString toJSON
        toLocaleDateString toLocaleString toLocaleTimeString toString
        toTimeString toUTCString valueOf)
      (js-lookup/category Function
        apply arguments bind call caller constructor length name toString)
      (js-lookup/category Number
        constructor toExponential toFixed toLocaleString toPrecision toString
        valueOf)
      (js-lookup/category Object
        __defineGetter__ __defineSetter__ __lookupGetter__ __lookupSetter__
        constructor hasOwnProperty isPrototypeOf propertyIsEnumerable
        toLocaleString toString valueOf)
      (js-lookup/category RegExp
        constructor exec global ignoreCase lastIndex multiline source test
        toString)
      (js-lookup/category String
        anchor big blink bold charAt charCodeAt concat constructor fixed
        fontcolor fontsize indexOf italics lastIndexOf length link
        localeCompare match replace search slice small split strike sub substr
        substring sup toLocaleLowerCase toLocaleUpperCase toLowerCase toString
        toUpperCase trim trimLeft trimRight valueOf)

      ;; Error Objects
      (js-lookup/category Error
        constructor message name toString)
      (js-lookup/category EvalError)
      (js-lookup/category RangeError)
      (js-lookup/category ReferenceError)
      (js-lookup/category SyntaxError)
      (js-lookup/category TypeError)
      (js-lookup/category URIError)

      ;; Other Objects
      (js-lookup/category Infinity
        NEGATIVE_INFINITY POSITIVE_INFINITY)
      (js-lookup/category JSON
        parse stringify)
      (js-lookup/category Math
        E LN10 LN2 LOG10E LOG2E PI SQRT1_2 SQRT2 abs acos asin atan atan2 ceil
        cos exp floor log max min pow random round sin sqrt tan)
      (js-lookup/category NaN)
      (js-lookup/category undefined)

      ;; Non-contructor Functions
      (js-lookup/category decodeURI)
      (js-lookup/category decodeURIComponent)
      (js-lookup/category encodeURI)
      (js-lookup/category encodeURIComponent)
      (js-lookup/category eval)
      (js-lookup/category isFinite)
      (js-lookup/category isNaN)
      (js-lookup/category parseFloat)
      (js-lookup/category parseInt))

    ;; Functions and Scope
    (js-lookup/root "Functions_and_function_scope/"
      (js-lookup/category arguments
        callee caller length)))

  ;; Typed Arrays -- this section on MDN is mostly empty
  (js-lookup/root "JavaScript_typed_arrays/"
    (js-lookup/category ArrayBuffer)
    (js-lookup/category DataView)
    (js-lookup/category Float32Array)
    (js-lookup/category Float64Array)
    (js-lookup/category Int16Array)
    (js-lookup/category Int32Array)
    (js-lookup/category Int8Array)
    (js-lookup/category Uint16Array)
    (js-lookup/category Uint32Array)
    (js-lookup/category Uint8Array)
    (js-lookup/category Uint8ClampedArray))

  (js-lookup/root "Mozilla_event_reference/"
    (js-lookup/category mousedown)
    (js-lookup/category mouseup)
    (js-lookup/category click)
    (js-lookup/category dblclick)
    (js-lookup/category mousemove)
    (js-lookup/category mouseover)
    (js-lookup/category mouseout)
    (js-lookup/category mouseenter)
    (js-lookup/category mouseleave)
    (js-lookup/category contextmenu)
    (js-lookup/category show)
    (js-lookup/category wheel)
    (js-lookup/category touchstart)
    (js-lookup/category touchend)
    (js-lookup/category touchmove)
    (js-lookup/category touchcancel)
    (js-lookup/category touchenter)
    (js-lookup/category touchleave)
    (js-lookup/category keydown)
    (js-lookup/category keyup)
    (js-lookup/category keypress)
    (js-lookup/category compositonstart)
    (js-lookup/category compositionupdate)
    (js-lookup/category compositionend)
    (js-lookup/category input)
    (js-lookup/category change)
    (js-lookup/category reset)
    (js-lookup/category submit)
    (js-lookup/category invalid)
    (js-lookup/category focus)
    (js-lookup/category blur)
    (js-lookup/category focusin)
    (js-lookup/category focusout)
    (js-lookup/category readystatechange)
    (js-lookup/category load)
    (js-lookup/category beforeunload)
    (js-lookup/category unload)
    (js-lookup/category abort)
    (js-lookup/category error)
    (js-lookup/category select)
    (js-lookup/category resize)
    (js-lookup/category scroll)
    (js-lookup/category hashchange)
    (js-lookup/category popstate)
    (js-lookup/category visibilitychange)
    (js-lookup/category pageshow)
    (js-lookup/category pagehide)
    (js-lookup/category drag)
    (js-lookup/category dragstart)
    (js-lookup/category dragend)
    (js-lookup/category dragover)
    (js-lookup/category dragenter)
    (js-lookup/category dragleave)
    (js-lookup/category drop)
    (js-lookup/category offline)
    (js-lookup/category online)
    (js-lookup/category copy)
    (js-lookup/category cut)
    (js-lookup/category paste)
    (js-lookup/category SVGLoad)
    (js-lookup/category SVGUnload)
    (js-lookup/category SVGAbort)
    (js-lookup/category SVGError)
    (js-lookup/category SVGResize)
    (js-lookup/category SVGScroll)
    (js-lookup/category SVGZoom)
    (js-lookup/category beginEvent)
    (js-lookup/category endEvent)
    (js-lookup/category repeatEvent)
    (js-lookup/category loadstart)
    (js-lookup/category progress)
    (js-lookup/category error)
    (js-lookup/category abort)
    (js-lookup/category load)
    (js-lookup/category loadend)
    (js-lookup/category suspend)
    (js-lookup/category emptied)
    (js-lookup/category stalled)
    (js-lookup/category loadedmetadata)
    (js-lookup/category loadeddata)
    (js-lookup/category canplay)
    (js-lookup/category canplaythrough)
    (js-lookup/category playing)
    (js-lookup/category waiting)
    (js-lookup/category seeking)
    (js-lookup/category seeked)
    (js-lookup/category ended)
    (js-lookup/category durationchange)
    (js-lookup/category timeupdate)
    (js-lookup/category play)
    (js-lookup/category pause)
    (js-lookup/category ratechange)
    (js-lookup/category volumechange)
    (js-lookup/category transitionend)
    (js-lookup/category animationstart)
    (js-lookup/category animationend)
    (js-lookup/category animationiteration)
    (js-lookup/category devicelight)
    (js-lookup/category devicemotion)
    (js-lookup/category deviceorientation)
    (js-lookup/category compassneedscalibration)
    (js-lookup/category deviceproximity)
    (js-lookup/category userproximity)
    (js-lookup/category devicetemperature)
    (js-lookup/category devicepressure)
    (js-lookup/category devicehumidity)
    (js-lookup/category devicenoise)
    (js-lookup/category chargingchange)
    (js-lookup/category chargingtimechange)
    (js-lookup/category dischargingtimechange)
    (js-lookup/category levelchange)
    (js-lookup/category orientationchange)
    (js-lookup/category fullscreenchange)
    (js-lookup/category fullscreenerror)
    (js-lookup/category pointerlockchange)
    (js-lookup/category pointerlockerror)
    (js-lookup/category gamepadconnected)
    (js-lookup/category gamepaddisconnected)
    (js-lookup/category beforeprint)
    (js-lookup/category afterprint)
    (js-lookup/category storage)
    (js-lookup/category checking)
    (js-lookup/category noupdate)
    (js-lookup/category downloading)
    (js-lookup/category progress)
    (js-lookup/category cached)
    (js-lookup/category updateready)
    (js-lookup/category obsolete)
    (js-lookup/category error)
    (js-lookup/category open)
    (js-lookup/category close)
    (js-lookup/category error)
    (js-lookup/category message)
    (js-lookup/category message)
    (js-lookup/category message)
    (js-lookup/category message)
    (js-lookup/category open)
    (js-lookup/category error)
    (js-lookup/category success)
    (js-lookup/category error)
    (js-lookup/category abort)
    (js-lookup/category complete)
    (js-lookup/category upgradeneeded)
    (js-lookup/category blocked)
    (js-lookup/category versionchange)
    (js-lookup/category onincoming)
    (js-lookup/category oncallschanged)
    (js-lookup/category onstatechange)
    (js-lookup/category ondialing)
    (js-lookup/category onalerting)
    (js-lookup/category onbusy)
    (js-lookup/category onconnecting)
    (js-lookup/category onconnected)
    (js-lookup/category ondisconnecting)
    (js-lookup/category ondisconnected)
    (js-lookup/category onholding)
    (js-lookup/category onheld)
    (js-lookup/category onresuming)
    (js-lookup/category onerror)
    (js-lookup/category ondelivered)
    (js-lookup/category onreceived)
    (js-lookup/category onsent)))

;;; js-lookup-database.el ends here
