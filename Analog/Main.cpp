#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/UART.h>
#include <AVR/Delay.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void setup()
{
    adc::setup<128>();
    UART::setup<9600>();
}

void loop()
{
    uint16_t x = adc::read<0>();

    printf("%d\n", x);

    delay_ms(100);
}

