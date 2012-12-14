;; When editing this file, it might help to eval these first.
;;
;;   (defmacro root (url &rest body)
;;     (declare (indent defun)))
;;
;;   (defmacro category (url &rest body)
;;     (declare (indent defun)))

(root "https://developer.mozilla.org/en-US/docs/"
  (root "JavaScript/Reference/Global_Objects/"
    (category Array
      concat constructor every filter forEach indexOf join lastIndexOf length
      map pop push reduce reduceRight reverse shift slice some sort splice
      toLocaleString toString unshift)
    (category Boolean
      constructor toString valueOf)
    (category Date
      constructor getDate getDay getFullYear getHours getMilliseconds
      getMinutes getMonth getSeconds getTime getTimezoneOffset getUTCDate
      getUTCDay getUTCFullYear getUTCHours getUTCMilliseconds getUTCMinutes
      getUTCMonth getUTCSeconds getYear setDate setFullYear setHours
      setMilliseconds setMinutes setMonth setSeconds setTime setUTCDate
      setUTCFullYear setUTCHours setUTCMilliseconds setUTCMinutes setUTCMonth
      setUTCSeconds setYear toDateString toGMTString toISOString toJSON
      toLocaleDateString toLocaleString toLocaleTimeString toString
      toTimeString toUTCString valueOf)
    (category Function
      apply arguments bind call caller constructor length name toString)
    (category Number
      constructor toExponential toFixed toLocaleString toPrecision toString
      valueOf)
    (category Object
      __defineGetter__ __defineSetter__ __lookupGetter__ __lookupSetter__
      constructor hasOwnProperty isPrototypeOf propertyIsEnumerable
      toLocaleString toString valueOf)
    (category RegExp
      compile constructor exec global ignoreCase lastIndex multiline source
      test toString)
    (category String
      anchor big blink bold charAt charCodeAt concat constructor fixed
      fontcolor fontsize indexOf italics lastIndexOf length link localeCompare
      match replace search slice small split strike sub substr substring sup
      toLocaleLowerCase toLocaleUpperCase toLowerCase toString toUpperCase trim
      trimLeft trimRight valueOf)))
