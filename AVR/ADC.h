#ifndef ADC_H
#define ADC_H

enum ref_source_t
	{ ref_source_vcc		= 0
	, ref_source_aref		= (1 << REFS0)
	, ref_source_1_1		= (1 << REFS1)
	, ref_source_2_56		= (1 << REFS2) | (1 << REFS1)
	, ref_source_2_56_cap	= (1 << REFS2) | (1 << REFS1) | (1 << REFS0)
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
		ADCSRA |= ((1 << ADPS1) | (1 << ADPS0));	// ADC prescale 8 (1MHz / 8 = 125kHz)
		ADCSRA |= ((1 << ADEN) | (1 << ADSC));		// start ADC and make initial conversion
	}

	template<class CH>
	static uint16_t read()
	{
		uint8_t n = ((ADMUX & 0xf0) != CH::ref_source) ? 2 : 1;

		ADMUX = CH::ref_source | CH::channel;

		for (uint8_t i = 0; i < n; ++i)
		{
			ADCSRA |= (1 << ADSC);	// start conversion
			while (ADCSRA & (1 << ADSC))
				/* wait for conversion */ ;
		}

		return ADCW;
	}

	static int8_t temp()
	{
		return adc::read<analog_input_t<15, ref_source_1_1>>() - 273;
	}
};

#endif // ADC_H

