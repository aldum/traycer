package utils

object MatchUtils:

  class OutOfBounds(b: Int):
    def unapply(arg: Int): Boolean = arg < 0 || arg >= b
