#ifndef ADC_H
#define ADC_H
#endif // ADC_H

template<unsigned CH>
struct analog_input_t
{
	static_assert(CH < 16, "adc channel out of range");

	static const int channel = CH;
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
		ADMUX = (ADMUX & 0xf0) | CH::channel;
		ADCSRA |= (1 << ADSC);	// start conversion
		while (ADCSRA & (1 << ADSC))
			/* wait for conversion */ ;
		return ADCW;
	}

	static float temp()
	{
		return adc::read<analog_input_t<15>>();
	}
};


