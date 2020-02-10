package com.knoldus

import org.scalatest._

class CurrencyConverterSpec extends FlatSpec with BeforeAndAfterAll {

  var currencyConverter: CurrencyConverter = new CurrencyConverter

  override def beforeAll(): Unit = {
    currencyConverter = new CurrencyConverter
  }

  override def afterAll(): Unit = {
    if (currencyConverter != null)
      currencyConverter = null
  }

  //USD test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to USD " in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "USD"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }

  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion  to INR " in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "INR"
    val expectedAmount = 715.1999999999999
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to EUR " in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "EUR"
    val expectedAmount = 9.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "AUD"
    val expectedAmount = 15.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to CAD" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "CAD"
    val expectedAmount = 13.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "YEN"
    val expectedAmount = 1097.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "KWD"
    val expectedAmount = 3.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to BHD " in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "BHD"
    val expectedAmount = 3.8
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to OMR" in {
    val amount: Double = 10

    val currencyFrom: String = "USD"
    val currencyTo: String = "OMR"
    val expectedAmount = 3.8
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "JOD"
    val expectedAmount = 7.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion to GBP" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "GBP"
    val expectedAmount = 7.800000000000001
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }

  "validateAndConvertCurrency method " should "throw exception as XYZ is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "USD"
    val currencyTo: String = "XYZ"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }
  "validateAndConvertCurrency method " should "throw exception as XYA is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "XYA"
    val currencyTo: String = "USD"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported from this currency.")
    }
  }
  "validateAndConvertCurrency method " should "throw exception as amount is -ve" in {
    val amount: Double = -10
    val currencyFrom: String = "USD"
    val currencyTo: String = "JOD"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency amount cannot be less than 0.")
    }
  }


  //INR test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to USD " in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "USD"
    val expectedAmount = 0.14
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)

    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to INR" in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "INR"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to EUR" in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "EUR"
    val expectedAmount = 0.13
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to AUD " in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "AUD"
    val expectedAmount = 0.21000000000000002
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to CAD " in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "CAD"
    val expectedAmount = 0.19
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "YEN"
    val expectedAmount = 15.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to KWD " in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "KWD"
    val expectedAmount = 0.043
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to BHD " in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "BHD"
    val expectedAmount = 0.053
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to OMR" in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "OMR"
    val expectedAmount = 0.054000000000000006
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "JOD"
    val expectedAmount = 0.099
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion INR to GBP" in {
    val amount: Double = 10
    val currencyFrom: String = "INR"
    val currencyTo: String = "GBP"
    val expectedAmount = 0.10999999999999999
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYB is not supported" in {
    val amount: Double = 10
    val currencyTo: String = "XYB"
    val currencyFrom: String = "INR"

    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //EUR test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to USD" in {
    val amount: Double = 10
    val currencyTo: String = "USD"
    val currencyFrom: String = "EUR"
    val expectedAmount = 10.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to INR" in {
    val amount: Double = 10
    val currencyTo: String = "INR"
    val currencyFrom: String = "EUR"
    val expectedAmount = 782.8
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to EUR" in {
    val amount: Double = 10
    val currencyTo: String = "EUR"
    val currencyFrom: String = "EUR"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to AUD" in {
    val amount: Double = 10
    val currencyTo: String = "AUD"
    val currencyFrom: String = "EUR"
    val expectedAmount = 16.4
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to CAD" in {
    val amount: Double = 10
    val currencyTo: String = "CAD"
    val currencyFrom: String = "EUR"
    val expectedAmount = 14.6
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "EUR"
    val currencyTo: String = "YEN"
    val expectedAmount = 1201.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "EUR"
    val currencyTo: String = "KWD"
    val expectedAmount = 3.3000000000000003
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to BHD" in {
    val amount: Double = 10
    val currencyFrom: String = "EUR"
    val currencyTo: String = "BHD"
    val expectedAmount = 4.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to OMR" in {
    val amount: Double = 10
    val currencyTo: String = "OMR"
    val currencyFrom: String = "EUR"
    val expectedAmount = 4.2
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to JOD " in {
    val amount: Double = 10
    val currencyTo: String = "JOD"
    val currencyFrom: String = "EUR"
    val expectedAmount = 7.800000000000001
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion EUR to GBP" in {
    val amount: Double = 10
    val currencyTo: String = "GBP"
    val currencyFrom: String = "EUR"
    val expectedAmount = 8.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYC is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "EUR"
    val currencyTo: String = "XYC"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //AUD test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to USD" in {
    val amount: Double = 10
    val currencyFrom: String = "AUD"
    val currencyTo: String = "USD"
    val expectedAmount = 6.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to INR" in {
    val amount: Double = 10
    val currencyFrom: String = "AUD"
    val currencyTo: String = "INR"
    val expectedAmount = 477.2
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to EUR" in {
    val amount: Double = 10
    val currencyFrom: String = "AUD"
    val currencyTo: String = "EUR"
    val expectedAmount = 6.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "AUD"
    val currencyTo: String = "AUD"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to CAD" in {
    val amount: Double = 10
    val currencyTo: String = "CAD"
    val currencyFrom: String = "AUD"
    val expectedAmount = 8.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to YEN" in {
    val amount: Double = 10
    val currencyTo: String = "YEN"
    val currencyFrom: String = "AUD"
    val expectedAmount = 732.4
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to KWD " in {
    val amount: Double = 10
    val currencyTo: String = "KWD"
    val currencyFrom: String = "AUD"
    val expectedAmount = 2.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to BHD" in {
    val amount: Double = 10
    val currencyTo: String = "BHD"
    val currencyFrom: String = "AUD"
    val expectedAmount = 2.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to OMR" in {
    val amount: Double = 10
    val currencyTo: String = "OMR"
    val currencyFrom: String = "AUD"
    val expectedAmount = 2.6
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "AUD"
    val currencyTo: String = "JOD"
    val expectedAmount = 4.699999999999999
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion AUD to GBP" in {
    val amount: Double = 10
    val currencyTo: String = "GBP"
    val currencyFrom: String = "AUD"
    val expectedAmount = 5.2
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYD is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "AUD"
    val currencyTo: String = "XYD"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //CAD test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to USD" in {
    val amount: Double = 10
    val currencyTo: String = "USD"
    val currencyFrom: String = "CAD"
    val expectedAmount = 7.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to INR" in {
    val amount: Double = 10
    val currencyTo: String = "INR"
    val currencyFrom: String = "CAD"
    val expectedAmount = 537.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to EUR" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "EUR"
    val expectedAmount = 6.8999999999999995
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "AUD"
    val expectedAmount = 11.299999999999999
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to CAD" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "CAD"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to  YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "YEN"
    val expectedAmount = 824.4
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to  KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "KWD"
    val expectedAmount = 2.3000000000000003
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to  BHD" in {

    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "BHD"
    val expectedAmount = 2.8000000000000003
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to OMR" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "OMR"
    val expectedAmount = 2.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to JOD " in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "JOD"
    val expectedAmount = 5.300000000000001
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion CAD to GRP" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "GBP"
    val expectedAmount = 5.8
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYE is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "CAD"
    val currencyTo: String = "XYE"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //YEN test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to USD" in {
    val amount: Double = 10
    val currencyTo: String = "USD"
    val currencyFrom: String = "YEN"
    val expectedAmount = 0.091
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to INR" in {
    val amount: Double = 10
    val currencyTo: String = "INR"
    val currencyFrom: String = "YEN"
    val expectedAmount = 6.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to EUR " in {
    val amount: Double = 10
    val currencyTo: String = "EUR"
    val currencyFrom: String = "YEN"
    val expectedAmount = 0.083
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to AUD" in {

    val amount: Double = 10
    val currencyTo: String = "AUD"
    val currencyFrom: String = "YEN"
    val expectedAmount = 0.14
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to CAD " in {
    val amount: Double = 10
    val currencyTo: String = "CAD"
    val currencyFrom: String = "YEN"
    val expectedAmount = 0.12
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to YEN " in {
    val amount: Double = 10
    val currencyFrom: String = "YEN"
    val currencyTo: String = "YEN"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "YEN"
    val currencyTo: String = "KWD"
    val expectedAmount = 0.028
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to BHD" in {
    val amount: Double = 10
    val currencyFrom: String = "YEN"
    val currencyTo: String = "BHD"
    val expectedAmount = 0.033999999999999996
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to OMR" in {
    val amount: Double = 10
    val currencyTo: String = "OMR"
    val currencyFrom: String = "YEN"
    val expectedAmount = 0.035
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to JOD" in {
    val amount: Double = 10
    val currencyTo: String = "JOD"
    val currencyFrom: String = "YEN"
    val expectedAmount = 0.065
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion YEN to GBP" in {
    val amount: Double = 10
    val currencyFrom: String = "YEN"
    val currencyTo: String = "GBP"
    val expectedAmount = 0.07100000000000001
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYF is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "YEN"
    val currencyTo: String = "XYF"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //KWD test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to USD " in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "USD"
    val expectedAmount = 32.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to INR" in {
    val currencyFrom: String = "KWD"
    val amount: Double = 10
    val currencyTo: String = "INR"
    val expectedAmount = 2350.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to EUR " in {
    val currencyFrom: String = "KWD"
    val amount: Double = 10
    val currencyTo: String = "EUR"
    val expectedAmount = 30.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "AUD"
    val expectedAmount = 49.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to CAD" in {

    val currencyFrom: String = "KWD"
    val amount: Double = 10
    val currencyTo: String = "CAD"
    val expectedAmount = 43.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to YEN" in {
    val currencyFrom: String = "KWD"
    val amount: Double = 10
    val currencyTo: String = "YEN"
    val expectedAmount = 3607.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "KWD"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to BHD" in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "BHD"
    val expectedAmount = 12.4
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to OMR" in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "OMR"
    val expectedAmount = 12.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "JOD"
    val expectedAmount = 23.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion KWD to GBP" in {
    val amount: Double = 10
    val currencyFrom: String = "KWD"
    val currencyTo: String = "GBP"
    val expectedAmount = 25.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYG is not supported" in {
    val currencyFrom: String = "KWD"
    val amount: Double = 10
    val currencyTo: String = "XYG"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //BHD test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO USD" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "USD"
    val expectedAmount = 26.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO INR" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "INR"
    val expectedAmount = 1897.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO EUR" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "EUR"
    val expectedAmount = 24.2
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "AUD"
    val expectedAmount = 39.8
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO CAD" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "CAD"
    val expectedAmount = 35.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "YEN"
    val expectedAmount = 2910.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "KWD"
    val expectedAmount = 8.100000000000001
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO BHD" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "BHD"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO OMR" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "OMR"
    val expectedAmount = 10.2
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "JOD"
    val expectedAmount = 18.799999999999997
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion BHD TO GBP" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "GBP"
    val expectedAmount = 20.6
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYH is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "BHD"
    val currencyTo: String = "XYH"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //OMR test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO USD " in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "USD"
    val expectedAmount = 26.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO INR " in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "INR"
    val expectedAmount = 1857.8
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO EUR" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "EUR"
    val expectedAmount = 23.700000000000003
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "AUD"
    val expectedAmount = 38.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO CAD" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "CAD"
    val expectedAmount = 34.6
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "YEN"
    val expectedAmount = 2850.5
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "KWD"
    val expectedAmount = 7.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO BHD" in {
    val amount: Double = 10
    val currencyTo: String = "BHD"
    val expectedAmount = 9.8
    val currencyFrom: String = "OMR"
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO OMR" in {
    val amount: Double = 10
    val currencyTo: String = "OMR"
    val currencyFrom: String = "OMR"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "JOD"
    val expectedAmount = 18.400000000000002
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion OMR TO GBP" in {
    val amount: Double = 10
    val currencyTo: String = "GBP"
    val currencyFrom: String = "OMR"
    val expectedAmount = 20.099999999999998
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYI is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "OMR"
    val currencyTo: String = "XYI"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //JOD test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO USD" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "USD"
    val expectedAmount = 14.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO INR" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "INR"
    val expectedAmount = 1008.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO  EUR" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "EUR"
    val expectedAmount = 12.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO AUD" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "AUD"
    val expectedAmount = 21.099999999999998
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO CAD" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "CAD"
    val expectedAmount = 18.799999999999997
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO YEN" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "YEN"
    val expectedAmount = 1547.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "KWD"
    val expectedAmount = 4.3
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO BHD" in {
    val amount: Double = 10
    val currencyTo: String = "BHD"
    val currencyFrom: String = "JOD"
    val expectedAmount = 5.300000000000001
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO OMR" in {
    val amount: Double = 10
    val currencyTo: String = "OMR"
    val currencyFrom: String = "JOD"
    val expectedAmount = 5.4
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "JOD"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion JOD TO GBP" in {
    val amount: Double = 10
    val currencyTo: String = "GBP"
    val currencyFrom: String = "JOD"
    val expectedAmount = 10.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYJ is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "JOD"
    val currencyTo: String = "XYJ"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }

  //GBP test cases
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO USD " in {
    val amount: Double = 12.9
    val currencyTo: String = "USD"
    val currencyFrom: String = "GBP"
    val expectedAmount = 16.641000000000002
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO  INR" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "INR"
    val expectedAmount = 922.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO EUR" in {
    val amount: Double = 10
    val currencyTo: String = "EUR"
    val currencyFrom: String = "GBP"
    val expectedAmount = 11.799999999999999
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO AUD" in {
    val amount: Double = 10
    val currencyTo: String = "AUD"
    val expectedAmount = 19.3
    val currencyFrom: String = "GBP"
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO CAD" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "CAD"
    val expectedAmount = 17.2
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO YEN" in {
    val amount: Double = 10
    val currencyTo: String = "YEN"
    val currencyFrom: String = "GBP"
    val expectedAmount = 1414.7
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO KWD" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "KWD"
    val expectedAmount = 3.9000000000000004
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO BHD" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "BHD"
    val expectedAmount = 4.9
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO OMR" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "OMR"
    val expectedAmount = 5.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO JOD" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "JOD"
    val expectedAmount = 9.1
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "validate the amount and currency given and perform conversion GBP TO  GBP" in {
    val amount: Double = 10
    val currencyTo: String = "GBP"
    val currencyFrom: String = "GBP"
    val expectedAmount = 10.0
    val actualAmount = currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    assert(expectedAmount == actualAmount)
  }
  "validateAndConvertCurrency method " should "throw exception as XYK is not supported" in {
    val amount: Double = 10
    val currencyFrom: String = "GBP"
    val currencyTo: String = "XYK"
    try {
      currencyConverter.validateAndConvertCurrency(currencyFrom, currencyTo, amount)
    } catch {
      case exception: Exception => assert(exception.getMessage == "Currency conversion not supported for this currency.")
    }
  }
}
    

