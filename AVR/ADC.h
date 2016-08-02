#pragma once

#include <avr/io.h>
#include "Delay.h"

template<int PRESCALE>
struct adc_prescale_traits
{
    static const uint8_t mask = _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
    static const uint8_t bits;
};

template<> struct adc_prescale_traits<2>   { static const uint8_t bits =                           _BV(ADPS0); };
template<> struct adc_prescale_traits<4>   { static const uint8_t bits =              _BV(ADPS1)             ; };
template<> struct adc_prescale_traits<8>   { static const uint8_t bits =              _BV(ADPS1) | _BV(ADPS0); };
template<> struct adc_prescale_traits<16>  { static const uint8_t bits = _BV(ADPS2)                          ; };
template<> struct adc_prescale_traits<32>  { static const uint8_t bits = _BV(ADPS2) |              _BV(ADPS0); };
template<> struct adc_prescale_traits<64>  { static const uint8_t bits = _BV(ADPS2) | _BV(ADPS1)             ; };
template<> struct adc_prescale_traits<128> { static const uint8_t bits = _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0); };

enum adc_ref_t
    { adc_ref_vcc           // Vcc as voltage reference, not connected to AREF
    , adc_ref_aref          // external voltage reference connected to AREF
    , adc_ref_1_1           // internal 1.1V voltage reference
    , adc_ref_1_1_cap       // internal 1.1V voltage reference with bypass capacitor at AREF
    , adc_ref_2_56          // internal 2.56V voltage reference
    , adc_ref_2_56_cap      // internal 2.56V voltage reference with bypass capacitor at AREF
    };

template<adc_ref_t REF>
struct adc_mux_ref_t
{
    static const uint8_t bits;
};

#if defined(__AVR_ATtiny85__)
static const uint8_t adc_mux_mux_mask = _BV(MUX3) | _BV(MUX2) | _BV(MUX1) | _BV(MUX0);
static const uint8_t adc_mux_srb_mask = 0;
static const uint8_t adc_mux_ref_mask = _BV(REFS2) | _BV(REFS1) | _BV(REFS0);
template<> struct adc_mux_ref_t<adc_ref_vcc>      { static const uint8_t bits =                                    0; };
template<> struct adc_mux_ref_t<adc_ref_aref>     { static const uint8_t bits =                           _BV(REFS0); };
template<> struct adc_mux_ref_t<adc_ref_1_1>      { static const uint8_t bits =              _BV(REFS1)             ; };
template<> struct adc_mux_ref_t<adc_ref_2_56>     { static const uint8_t bits = _BV(REFS2) | _BV(REFS1)             ; };
template<> struct adc_mux_ref_t<adc_ref_2_56_cap> { static const uint8_t bits = _BV(REFS2) | _BV(REFS1) | _BV(REFS0); };
static const uint8_t adc_temp_channel = 15;
static const adc_ref_t adc_temp_ref = adc_ref_1_1;
#elif defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__)
static const uint8_t adc_mux_mux_mask = _BV(MUX3) | _BV(MUX2) | _BV(MUX1) | _BV(MUX0);
static const uint8_t adc_mux_srb_mask = 0;
static const uint8_t adc_mux_ref_mask = _BV(REFS1) | _BV(REFS0);
template<> struct adc_mux_ref_t<adc_ref_aref>     { static const uint8_t bits =                                    0; };
template<> struct adc_mux_ref_t<adc_ref_vcc>      { static const uint8_t bits =                           _BV(REFS0); };
template<> struct adc_mux_ref_t<adc_ref_1_1_cap>  { static const uint8_t bits =              _BV(REFS1) | _BV(REFS0); };
static const uint8_t adc_temp_channel = 8;
static const adc_ref_t adc_temp_ref = adc_ref_1_1_cap;
#elif defined(__AVR_ATmega32U4__)
static const uint8_t adc_mux_mux_mask = _BV(MUX4) | _BV(MUX3) | _BV(MUX2) | _BV(MUX1) | _BV(MUX0);
static const uint8_t adc_mux_srb_mask = _BV(MUX5);
static const uint8_t adc_mux_ref_mask = _BV(REFS1) | _BV(REFS0);
template<> struct adc_mux_ref_t<adc_ref_aref>     { static const uint8_t bits =                                    0; };
template<> struct adc_mux_ref_t<adc_ref_vcc>      { static const uint8_t bits =                           _BV(REFS0); };
template<> struct adc_mux_ref_t<adc_ref_2_56_cap> { static const uint8_t bits =              _BV(REFS1) | _BV(REFS0); };
static const uint8_t adc_temp_channel = 0x27;
static const adc_ref_t adc_temp_ref = adc_ref_2_56_cap;
#endif

template<uint8_t CH, uint8_t RS = adc_ref_vcc>
struct analog_input_t
{
	static_assert(CH < 64, "adc channel out of range");

	static const uint8_t channel = CH;
	static const uint8_t ref_source = RS;
};

struct adc
{
    template<int PRESCALE>
	static void setup()
	{
        ADCSRA = adc_prescale_traits<PRESCALE>::bits;
		ADCSRA |= _BV(ADEN) | _BV(ADSC);		        // start ADC and make initial conversion
	}

    template<uint8_t CH, adc_ref_t REF = adc_ref_vcc>
    static uint16_t read()
    {
        static_assert((CH & ~(adc_mux_mux_mask | adc_mux_srb_mask)) == 0, "adc channel out of range");
        bool vref_changed = (ADMUX & adc_mux_ref_mask) != adc_mux_ref_t<REF>::bits;

        ADMUX = adc_mux_ref_t<REF>::bits | (CH & adc_mux_mux_mask);
        if (adc_mux_srb_mask)                           // branch resolved statically
            ADCSRB = (ADCSRB & ~adc_mux_srb_mask) | (CH & adc_mux_srb_mask);
        if (vref_changed)                               // stabilize after voltage reference change
            delay_ms(2);
		ADCSRA |= _BV(ADSC);	                        // start conversion
		while (ADCSRA & _BV(ADSC))
			;                                           // wait for completion
		return ADCW;
    }

	static int8_t temp()
	{
		return adc::read<adc_temp_channel, adc_temp_ref>() - 273;
	}
};

