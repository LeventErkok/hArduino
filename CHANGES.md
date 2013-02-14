* Hackage: (http://hackage.haskell.org/package/hArduino)
* GitHub:  (http://leventerkok.github.com/hArduino)

* Latest Hackage released version: 0.3

### Version 0.4, Not yet released
 
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
    * Add counter: use push buttons to count up and down
    * Add analog-reading example
    * Add LCD controller example
    * Add wiring schematics for all sample programs

### Version 0.2, 2013-01-28

 * Rewrite the communication engine
 * Digital input/output implementation
 * Add switch example

### Version 0.1, 2013-01-14

 * Initial design
 * Blink example operational
 * Created home page at: http://leventerkok.github.com/hArduino 
