* Hackage: (http://hackage.haskell.org/package/hArduino)
* GitHub:  (http://leventerkok.github.com/hArduino)

* Latest Hackage released version: 1.2

### Version 1.2, 2022-12-14
 * Make hArduino compile with recent versions of GHC

### Version 1.1, 2016-03-27
 * Unfortunately the Firmata project moved on, and hArduino no longer
   works with the latest release of StandardFirmata. This release mainly
   makes a note of this fact, and points to an older version of StandardFirmata
   that hArduino has been tested against. Volunteers encouraged to
   contribute to bring hArduino up-to-date: Contact me if interested!

   In the mean-time, use the following version of StandardFirmata, which is
   known to work with hArduino: (http://github.com/LeventErkok/hArduino/blob/master/StandardFirmata/StandardFirmata.ino)

 * Make the pin-capabilities code robust, by explicitly catching
   (and ignoring) capabilities we do not know/care about. Previously
   we were simply using toEnum, which choked with new versions
   of Firmata as they added new options. Thanks to Can Akcura on
   Github for pointing out the issue.


### Version 0.9, 2014-02-09
 * Added example program for Morse encoding.
   Original idea by Antoine Dumont. Thanks!

### Version 0.8, 2013-12-15
 * Add support for Piezo speakers
 * Add simple musical note playing support, and a
   jingle-bells playing example. (Not a great
   rendering, but still recognizable!)

### Version 0.7, 2013-11-09
 * Export LCD type, for ease of programming
 * Added the number guessing game using the OSEpp shield.
   Thanks to David Palmer for lending me his shield to play with!

### Version 0.6, 2013-03-08

 * Make hArduino Windows friendly by removing dependence
   on the unix package. Thanks to Andriy Drozdyuk for pointing
   out the Windows compilation issue. (Tested on Windows 7.)

### Version 0.5, 2013-03-07
 
 * New hardware components supported:
     * Shift-registers
     * Seven-segment displays
     * Servo-motors
 * New examples:
    * PulseIn: Demonstrates the use of reading pulses
    * PulseOut: Demonstrates the use of sending pulses
    * Distance: Measure the distance using an HC-SC04 sensor
    * Seven-segment: Display characters on a seven-segment display
    * Servo: Control a servo board
 * New functions:
     * pulseIn_hostTiming/pulseOut_hostTiming: Send and receive pulses.
         * NB. These functions use host-timing: watch out for accuracy
     * pulse: Send and receive a digital pulse on a pin.
         * This function is more accurate than the pair above, as
	   it uses a custom Firmata command to measure the pulse.
	   However, you need a custom Firmata version to use this
	   function, as the standard version that ships with Arduino
	   as of March 2013 does not support this functionality yet.
     * time: Measure the time taken by an Arduino action
     * timeOut: Run an action only for the given-time-out

### Version 0.4, 2013-03-05

 * Bugfix: Remove spurious extra call to user program
 * Rework pin assignment logic, making use of analog/digital pins much more clearer.
 * Better exception handling
 * Remove threadDelay workaround on the Mac. NB. If you are running on OSX, then
   you need at least GHC 7.6.2!
  
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
