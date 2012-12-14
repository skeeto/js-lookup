;;; js-lookup-database.el --- URL database for js-lookup

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; The s-expression below maps JavaScript concepts to URLs on the
;; Mozilla Developer Network (MDN) wiki.

;;; Code:

(require 'js-lookup)

(js-lookup/root "https://developer.mozilla.org/en-US/docs/"
  (js-lookup/root "JavaScript/Reference/Global_Objects/"
    (js-lookup/category Array
      concat constructor every filter forEach indexOf join lastIndexOf length
      map pop push reduce reduceRight reverse shift slice some sort splice
      toLocaleString toString unshift)
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
      compile constructor exec global ignoreCase lastIndex multiline source
      test toString)
    (js-lookup/category String
      anchor big blink bold charAt charCodeAt concat constructor fixed
      fontcolor fontsize indexOf italics lastIndexOf length link localeCompare
      match replace search slice small split strike sub substr substring sup
      toLocaleLowerCase toLocaleUpperCase toLowerCase toString toUpperCase trim
      trimLeft trimRight valueOf)))

;;; js-lookup-database.el ends here
