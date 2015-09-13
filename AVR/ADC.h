#pragma once

enum ref_source_t
#if defined(__AVR_ATtiny85__)
	{ ref_source_vcc		= 0
	, ref_source_aref		= _BV(REFS0)
	, ref_source_1_1		= _BV(REFS1)
	, ref_source_2_56		= _BV(REFS2) | _BV(REFS1)
	, ref_source_2_56_cap	= _BV(REFS2) | _BV(REFS1) | _BV(REFS0)
#elif defined(__AVR_ATmega328P__)
	{ ref_source_aref		= 0
	, ref_source_vcc		= _BV(REFS0)
	, ref_source_1_1		= _BV(REFS0) | _BV(REFS1)
#endif
	};

template<uint8_t CH, uint8_t RS = ref_source_vcc>
struct analog_input_t
{
	static_assert(CH < 16, "adc channel out of range");

	static const uint8_t channel = CH;
	static const uint8_t ref_source = RS;
};

struct adc
{
	static void setup()
	{
#if defined(__AVR_ATtiny85__)
		ADCSRA |= _BV(ADPS1) | _BV(ADPS0);	// ADC prescale 8 (1MHz / 8 = 125kHz)
#elif defined(__AVR_ATmega328P__)
		ADCSRA |= _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);	// ADC prescale 128 (16MHz / 128 = 125kHz)
#endif
		ADCSRA |= _BV(ADEN) | _BV(ADSC);		// start ADC and make initial conversion
	}

	template<class CH>
	static uint16_t read()
	{
		uint8_t n = ((ADMUX & 0xf0) != CH::ref_source) ? 2 : 1;

		ADMUX = CH::ref_source | CH::channel;

		for (uint8_t i = 0; i < n; ++i)
		{
			ADCSRA |= _BV(ADSC);	// start conversion
			while (ADCSRA & _BV(ADSC))
				/* wait for conversion */ ;
		}

		return ADCW;
	}

	static int8_t temp()
	{
		return adc::read<analog_input_t<15, ref_source_1_1>>() - 273;
	}
};

