    #include <avr/io.h>
    #include <util/delay.h>
    
    static const uint8_t f2000 = 249;
    static const uint8_t f2500 = 199;
    static const uint8_t f3000 = 166;
    
    void setup()
    {
        TCCR0A = _BV(WGM01) | _BV(COM0A0);  // CTC mode w toggle channel A on compare match
        TCCR0B = _BV(CS01);                 // clock select prescale = 8
        DDRB |= _BV(0);                     // enable channel A = OC0A (PB0) output
    }
    
    void loop()
    {
        static uint8_t i = 0;
    
        switch (++i & 0x3)
        {
        case 0: OCR0A = 249; break;     // 2kHz
        case 1: OCR0A = 199; break;     // 2.5kHz
        case 2: OCR0A = 166; break;     // 3kHz
        case 3: OCR0A = 199; break;     // 2.5kHz
        }
    
        _delay_ms(2000);
    }
    
    int main()
    {
    	setup();
    	for (;;)
    		loop();
    }
    
