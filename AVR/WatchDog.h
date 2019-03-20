#pragma once

#include <avr/wdt.h>

template<int KILOCYCLES> struct wdt_traits {};

template<> struct wdt_traits<2> { static const uint8_t prescaler = 0; };
template<> struct wdt_traits<4> { static const uint8_t prescaler = _BV(WDP0); };
template<> struct wdt_traits<8> { static const uint8_t prescaler = _BV(WDP1); };
template<> struct wdt_traits<16> { static const uint8_t prescaler = _BV(WDP1) | _BV(WDP0); };
template<> struct wdt_traits<32> { static const uint8_t prescaler = _BV(WDP2); };
template<> struct wdt_traits<64> { static const uint8_t prescaler = _BV(WDP2) | _BV(WDP0); };
template<> struct wdt_traits<128> { static const uint8_t prescaler = _BV(WDP2) | _BV(WDP1); };
template<> struct wdt_traits<256> { static const uint8_t prescaler = _BV(WDP2) | _BV(WDP1) | _BV(WDP0); };
template<> struct wdt_traits<512> { static const uint8_t prescaler = _BV(WDP3); };
template<> struct wdt_traits<1024> { static const uint8_t prescaler = _BV(WDP3) | _BV(WDP0); };

template<int KILOCYCLES>
void enable_watchdog()
{
    cli();
    MCUSR &= ~(_BV(WDRF));
    WDTCSR |= _BV(WDCE) | _BV(WDE);
    WDTCSR = _BV(WDE) | wdt_traits<KILOCYCLES>::prescaler;
    sei();
}

