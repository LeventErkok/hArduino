* Hackage: (http://hackage.haskell.org/package/hArduino)
* GitHub:  (http://leventerkok.github.com/hArduino)

* Latest Hackage released version: 0.3

### Version 0.4, Not yet released
 
 * New functions:
     * pulseIn: Measure the length of a pulse, with timeout
     * time: Measure the time taken by an Arduino action
     * timeOut: Run an action only for the given-time-out

 * Examples
    * Pulse: Demonstrates the use of pulse-reading
    * Distance: Demonstrates the use of 

 * Other:
    * Bugfix: Remove spurious extra call to user program
    * Rework pin assignment logic, making use of analog/digital
      pins much more clearer.
    * Better exception handling
    * Remove threadDelay workaround on the Mac. NB. If you are
      running on OSX, then you need at least GHC 7.6.2!

### Version 0.3, 2013-02-10

 * Library
    * Add support for pull-up resistors
    * Implement routines for waiting on digital triggers
    * Add support for reading analog values and setting sampling frequency.
    * Add support for LCDs (based on the Hitachi 44780 chip)
    * Better handling for Ctrl-C interrupts

 * Examples
    * Counter: Use push buttons to count up and down
    * Analog: Reading analog values
    * LCD: Control an LCD, writing text/glyphs etc

### Version 0.2, 2013-01-28

 * Library
    * Rewrite the communication engine
    * Digital input/output implementation

 * Examples
    * Button: Detecting putton pushes

### Version 0.1, 2013-01-14

 * Library
    * Initial design
    * Created home page at: http://leventerkok.github.com/hArduino 

 * Examples
    * Blink: Hello world!
