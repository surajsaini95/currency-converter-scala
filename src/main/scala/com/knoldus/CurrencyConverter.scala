package com.knoldus

/**
 * This is a currency converter class written in scala.
 * User can convert one currency into ten other currencies.
 * Currencies supported are :
 * USD
 * INR
 * EUR
 * AUD
 * CAD
 * YEN
 * KWD
 * BHD
 * OMR
 * JOD
 * GBP
 */
class CurrencyConverter {

  /** *
   * the method validateAndConvertCurrency first validates amount and currency parameters then performs conversion
   *
   * @param currencyFrom currency from which user want to convert currency
   * @param currencyTo   currency to which user want to convert currency
   * @param amount       amount from which user want to convert
   * @return an amount value after conversion
   */
  def validateAndConvertCurrency(currencyFrom: String, currencyTo: String, amount: Double): Double = {
    if (amount >= 0) {
      currencyFrom match {
        case "USD" => convertFromUSD(currencyTo, amount)
        case "INR" => convertFromINR(currencyTo, amount)
        case "EUR" => convertFromEUR(currencyTo, amount)
        case "AUD" => convertFromAUD(currencyTo, amount)
        case "CAD" => convertFromCAD(currencyTo, amount)
        case "YEN" => convertFromYEN(currencyTo, amount)
        case _ => validateCurrencyAgain(currencyFrom, currencyTo, amount)
      }
    }
    else {
      throw new Exception("Currency amount cannot be less than 0.")
    }
  }

  private def validateCurrencyAgain(currencyFrom: String, currencyTo: String, amount: Double): Double = {
    currencyFrom match {
      case "KWD" => convertFromKWD(currencyTo, amount)
      case "BHD" => convertFromBHD(currencyTo, amount)
      case "OMR" => convertFromOMR(currencyTo, amount)
      case "JOD" => convertFromJOD(currencyTo, amount)
      case "GBP" => convertFromGBP(currencyTo, amount)
      case _ => throw new Exception("Currency conversion not supported from this currency.")
    }
  }

  /**
   * convertFromUSD performs conversion from USD to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromUSD(currencyTo: String, amount: Double): Double = {
    currencyTo match {
      case "USD" => amount
      case "INR" => amount * 71.52
      case "EUR" => amount * 0.91
      case "AUD" => amount * 1.5
      case "CAD" => amount * 1.33
      case "YEN" => amount * 109.73
      case _ => convertAgainFromUSD(currencyTo, amount)
    }
  }

  private def convertAgainFromUSD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.3
    case "BHD" => amount * 0.38
    case "OMR" => amount * 0.38
    case "JOD" => amount * 0.71
    case "GBP" => amount * 0.78
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromINR performs conversion from INR to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromINR(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 0.014
    case "INR" => amount
    case "EUR" => amount * 0.013
    case "AUD" => amount * 0.021
    case "CAD" => amount * 0.019
    case "YEN" => amount * 1.53
    case _ => convertAgainFromINR(currencyTo, amount)
  }

  private def convertAgainFromINR(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.0043
    case "BHD" => amount * 0.0053
    case "OMR" => amount * 0.0054
    case "JOD" => amount * 0.0099
    case "GBP" => amount * 0.011
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromEUR performs conversion from EUR to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromEUR(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 1.09
    case "INR" => amount * 78.28
    case "EUR" => amount
    case "AUD" => amount * 1.64
    case "CAD" => amount * 1.46
    case "YEN" => amount * 120.11
    case _ => convertAgainFromEUR(currencyTo, amount)
  }

  private def convertAgainFromEUR(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.33
    case "BHD" => amount * 0.41
    case "OMR" => amount * 0.42
    case "JOD" => amount * 0.78
    case "GBP" => amount * 0.85
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromAUD performs conversion from AUD to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromAUD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 0.67
    case "INR" => amount * 47.72
    case "EUR" => amount * 0.61
    case "AUD" => amount
    case "CAD" => amount * 0.89
    case "YEN" => amount * 73.24
    case _ => convertAgainFromAUD(currencyTo, amount)
  }

  private def convertAgainFromAUD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.2
    case "BHD" => amount * 0.25
    case "OMR" => amount * 0.26
    case "JOD" => amount * 0.47
    case "GBP" => amount * 0.52
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromCAD performs conversion from CAD to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromCAD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 0.75
    case "INR" => amount * 53.73
    case "EUR" => amount * 0.69
    case "AUD" => amount * 1.13
    case "CAD" => amount
    case "YEN" => amount * 82.44

    case _ => convertAgainFromCAD(currencyTo, amount)
  }

  private def convertAgainFromCAD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.23
    case "BHD" => amount * 0.28
    case "OMR" => amount * 0.29
    case "JOD" => amount * 0.53
    case "GBP" => amount * 0.58
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromYEN performs conversion from YEN to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromYEN(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 0.0091
    case "INR" => amount * 0.65
    case "EUR" => amount * 0.0083
    case "AUD" => amount * 0.014
    case "CAD" => amount * 0.012
    case "YEN" => amount
    case _ => convertAgainFromYEN(currencyTo, amount)
  }

  private def convertAgainFromYEN(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.0028
    case "BHD" => amount * 0.0034
    case "OMR" => amount * 0.0035
    case "JOD" => amount * 0.0065
    case "GBP" => amount * 0.0071
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromKWD performs conversion from KWD to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromKWD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 3.29
    case "INR" => amount * 235.09
    case "EUR" => amount * 3
    case "AUD" => amount * 4.93
    case "CAD" => amount * 4.37
    case "YEN" => amount * 360.71
    case _ => convertAgainFromKWD(currencyTo, amount)
  }

  private def convertAgainFromKWD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount
    case "BHD" => amount * 1.24
    case "OMR" => amount * 1.27
    case "JOD" => amount * 2.33
    case "GBP" => amount * 2.55
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromBHD performs conversion from BHD to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromBHD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 2.65
    case "INR" => amount * 189.7
    case "EUR" => amount * 2.42
    case "AUD" => amount * 3.98
    case "CAD" => amount * 3.53
    case "YEN" => amount * 291.07
    case _ => convertAgainFromBHD(currencyTo, amount)
  }

  private def convertAgainFromBHD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.81
    case "BHD" => amount
    case "OMR" => amount * 1.02
    case "JOD" => amount * 1.88
    case "GBP" => amount * 2.06
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromOMR performs conversion from OMR to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromOMR(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 2.6
    case "INR" => amount * 185.78
    case "EUR" => amount * 2.37
    case "AUD" => amount * 3.89
    case "CAD" => amount * 3.46
    case "YEN" => amount * 285.05

    case _ => convertAgainFromOMR(currencyTo, amount)
  }

  private def convertAgainFromOMR(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.79
    case "BHD" => amount * 0.98
    case "OMR" => amount
    case "JOD" => amount * 1.84
    case "GBP" => amount * 2.01
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromJOD performs conversion from JOD to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromJOD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 1.41
    case "INR" => amount * 100.87
    case "EUR" => amount * 1.29
    case "AUD" => amount * 2.11
    case "CAD" => amount * 1.88
    case "YEN" => amount * 154.77
    case _ => convertAgainFromJOD(currencyTo, amount)
  }

  private def convertAgainFromJOD(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.43
    case "BHD" => amount * 0.53
    case "OMR" => amount * 0.54
    case "JOD" => amount
    case "GBP" => amount * 1.09
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }

  /**
   * convertFromGBP performs conversion from GBP to other currencies
   *
   * @param currencyTo currency to which user want to convert currency
   * @param amount     amount from which user want to convert
   * @return an amount value after conversion
   */
  def convertFromGBP(currencyTo: String, amount: Double): Double = currencyTo match {
    case "USD" => amount * 1.29
    case "INR" => amount * 92.2
    case "EUR" => amount * 1.18
    case "AUD" => amount * 1.93
    case "CAD" => amount * 1.72
    case "YEN" => amount * 141.47
    case _ => convertAgainFromGBP(currencyTo, amount)
  }

  private def convertAgainFromGBP(currencyTo: String, amount: Double): Double = currencyTo match {
    case "KWD" => amount * 0.39
    case "BHD" => amount * 0.49
    case "OMR" => amount * 0.5
    case "JOD" => amount * 0.91
    case "GBP" => amount
    case _ => throw new Exception("Currency conversion not supported for this currency.")
  }
}