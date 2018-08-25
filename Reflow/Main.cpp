#include <AVR/Main.h>
#include <Arduino/Pins.h>
#include <AVR/ADC.h>
#include <AVR/UART.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef D13 led;
typedef D3 ssr_a;
typedef D2 ssr_b;

template<int CH>
class thermocouple
{
public:
    static float voltage()
    {
        static const uint8_t n = 30;  // should be some kind of timed wait!
        static const uint8_t m = 10;
        uint16_t x = 0;
        float v = 0;

        for (uint8_t i = 0; i < n; ++i)
            x = adc::read<CH>();

        if (m_ref1_1)
            m_ref1_1 = x < 210;
        else
            m_ref1_1 = x < 200;

        if (m_ref1_1)
        {
            for (uint8_t i = 0; i < n; ++i)         // stabilize reference
                x = adc::read<CH, adc_ref_1_1_cap>();

            uint16_t t = 0;

            for (uint8_t i = 0; i < m; ++i)
                t += adc::read<CH, adc_ref_1_1_cap>();

            v = 1.1 * static_cast<float>(t) / (m * 1023);
        }
        else
        {
            uint16_t t = 0;

            for (uint8_t i = 0; i < m; ++i)
                t += adc::read<CH>();

            v = 4.72 * static_cast<float>(t) / (m * 1023);
        }

        return v;
    }

private:
    static bool m_ref1_1;
};

template<int CH>
bool thermocouple<CH>::m_ref1_1 = false;

typedef thermocouple<2> couple_a;
typedef thermocouple<1> couple_b;

void setup()
{
    led::setup();
    ssr_a::setup();
    ssr_b::setup();
    adc::setup<128>();
    UART::setup<19200>();
    printf("Marangisto Reflow 1.0\n");
}

void loop()
{
    static uint8_t i = 0;

    led::toggle();
 
    if ((i & 0x3) == 0)
        ssr_a::toggle();
    if ((i & 0x5) == 0)
        ssr_b::toggle();

    float v_a = couple_a::voltage();
    float v_b = couple_b::voltage();
    float t_a = v_a / 5e-3;
    float t_b = v_b / 5e-3;

    printf("%.2fV %.1fC %.2fV %.1fC\n", (double) v_a, (double) t_a, (double) v_b, (double) t_b);

    ++i;
    delay_ms(100);
}

