/**
 * JavaScript File (misc.js)
 * Created: Wed 15 Jun 2011 10:24:15 PM CEST
 *
 * This JavaScript source code was developped by François-Xavier Thomas.
 * You are free to copy, adapt or modify it.
 * If you do so, however, leave my name somewhere in the credits, I'd appreciate it ;)
 * 
 * @author François-Xavier Thomas <fx.thomas@gmail.com>
 * @version 1.0
 */

$(function() {
  $('body').delegate (".download", "mouseenter", function(ev) {
    $(this).children('.message').slideUp(100);
    $(this).children('.links').slideDown(100);
  });
  $('body').delegate (".download", "mouseleave", function(ev) {
    $(this).children('.message').slideDown(100);
    $(this).children('.links').slideUp(100);
  });
});
