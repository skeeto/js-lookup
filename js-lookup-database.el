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
    (js-lookup/entries
      SVGAbort SVGError SVGLoad SVGResize SVGScroll SVGUnload SVGZoom abort
      afterprint animationend animationiteration animationstart beforeprint
      beforeunload beginEvent blocked blur cached canplay canplaythrough
      change chargingchange chargingtimechange checking click close
      compassneedscalibration complete compositionend compositionupdate
      compositonstart contextmenu copy cut dblclick devicehumidity devicelight
      devicemotion devicenoise deviceorientation devicepressure
      deviceproximity devicetemperature dischargingtimechange downloading
      drag dragend dragenter dragleave dragover dragstart drop durationchange
      emptied endEvent ended error focus focusin focusout fullscreenchange
      fullscreenerror gamepadconnected gamepaddisconnected hashchange input
      invalid keydown keypress keyup levelchange load loadeddata
      loadedmetadata loadend loadstart mousedown mouseenter mouseleave
      mousemove mouseout mouseover mouseup noupdate obsolete offline
      onalerting onbusy oncallschanged onconnected onconnecting ondelivered
      ondialing ondisconnected ondisconnecting onerror onheld onholding
      onincoming online onreceived onresuming onsent onstatechange open
      orientationchange pagehide pageshow paste pause play playing
      pointerlockchange pointerlockerror popstate progress ratechange
      readystatechange repeatEvent reset resize scroll seeked seeking select
      show stalled storage submit success suspend timeupdate touchcancel
      touchend touchenter touchleave touchmove touchstart transitionend
      unload updateready upgradeneeded userproximity versionchange
      visibilitychange volumechange waiting wheel)))

;;; js-lookup-database.el ends here
