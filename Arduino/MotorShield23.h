#pragma once

#include <AVR/TWI.h>

enum command_t { ms_stop, ms_brake, ms_forward, ms_reverse };

template<int BRIDGE> struct bridge_traits {};

template<> struct bridge_traits<0>
{
    static const uint8_t pwm = 8;
    static const uint8_t in1 = 9;
    static const uint8_t in2 = 10;
};

template<> struct bridge_traits<1>
{
    static const uint8_t pwm = 13;
    static const uint8_t in1 = 12;
    static const uint8_t in2 = 11;
};

template<> struct bridge_traits<2>
{
    static const uint8_t pwm = 2;
    static const uint8_t in1 = 3;
    static const uint8_t in2 = 4;
};

template<> struct bridge_traits<3>
{
    static const uint8_t pwm = 7;
    static const uint8_t in1 = 6;
    static const uint8_t in2 = 5;
};

class bridge_t
{
public:
	typedef void (*duty_t)(uint16_t);
	typedef void (*cmd_t)(command_t);

    bridge_t(duty_t d, cmd_t c): m_duty(d), m_cmd(c) {}

    inline void duty_cycle(uint16_t d) { m_duty(d); }
    inline void command(command_t c) { m_cmd(c); }

private:
    duty_t m_duty;
    cmd_t m_cmd;
};

template<uint8_t I2C = 0x60>
class motor_shield_23_t
{
public:
    template<uint8_t PS = 0x03>
    static void setup()
    {
        twi_t::setup(100000);               // FIXME: do we want to do this separately?
        sei();                              // danger if other things are not ready!
        write(MODE1, 0x0);                  // initial settings

        uint8_t mode = read(MODE1);

        write(MODE1, (mode & 0x7F) | 0x10); // go to sleep
        write(PRESCALE, PS);                // set the prescaler
        write(MODE1, mode);
        delay_us(500);
        write(MODE1, mode | 0xa1);          // turn on auto increment

        for (uint8_t i = 0; i < 16; ++i)
            clr(i);
    }

    template<int BRIDGE>
    static void duty_cycle(uint16_t x)
    {
        if (x < 4096)
            set_pwm(bridge_traits<BRIDGE>::pwm, 0, x);
        else
            set_pwm(bridge_traits<BRIDGE>::pwm, 4096, 0);
    }

    template<int BRIDGE>
    static void command(command_t m)
    {
        switch (m)
        {
            case ms_forward: set(bridge_traits<BRIDGE>::in1); clr(bridge_traits<BRIDGE>::in2); break;
            case ms_reverse: clr(bridge_traits<BRIDGE>::in1); set(bridge_traits<BRIDGE>::in2); break;
            case ms_brake:   set(bridge_traits<BRIDGE>::in1); set(bridge_traits<BRIDGE>::in2); break;
            case ms_stop:    clr(bridge_traits<BRIDGE>::in1); clr(bridge_traits<BRIDGE>::in2); break;
        }
    }

    template<int BRIDGE>
    static bridge_t bridge()
    {
        return bridge_t(duty_cycle<BRIDGE>, command<BRIDGE>);
    }

private:
    static inline void set(uint8_t ch)
    {
        set_pwm(ch, 4096, 0);
    }

    static inline void clr(uint8_t ch)
    {
        set_pwm(ch, 0, 0);
    }

    static void set_pwm(uint8_t ch, uint16_t on, uint16_t off)
    {
        uint8_t buf[] = { 0x6 + (ch << 2), on, on >> 8, off, off >> 8 };

        TWI(twi_t::write(I2C, buf, sizeof(buf)));
    }

    // PCA9685 controls
    static const uint8_t MODE1 = 0x00;
    static const uint8_t MODE2 = 0x01;
    static const uint8_t PRESCALE = 0xfe;

    static uint8_t read(uint8_t addr)
    {
        uint8_t x;

        TWI(twi_t::write_read(I2C, &addr, 1, &x, 1));
        return x;
    }

    static void write(uint8_t addr, uint8_t x)
    {
        uint8_t buf[] = { addr, x };

        TWI(twi_t::write(I2C, buf, sizeof(buf)));
    }
};

