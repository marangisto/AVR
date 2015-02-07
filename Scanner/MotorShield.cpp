#include <Wire.h>
#include "MotorShield.h"

#define PCA9685_SUBADR1 0x2
#define PCA9685_SUBADR2 0x3
#define PCA9685_SUBADR3 0x4

#define PCA9685_MODE1 0x0
#define PCA9685_PRESCALE 0xFE

static const uint8_t apwm = 8, ain1 = 3, ain2 = 9;
static const uint8_t bpwm = 13, bin1 = 4, bin2 = 10;
static const uint8_t cpwm = 2, cin1 = 5, cin2 = 11;
static const uint8_t dpwm = 7, din1 = 6, din2 = 12;

MotorShieldV2::MotorShieldV2(uint8_t addr)
{
	m_i2caddr = addr;
	Wire.begin();
	reset();
}

void MotorShieldV2::reset(void) {
	write8(PCA9685_MODE1, 0x0);
	setPWMFreq(1600);
	for (uint8_t i=0; i<16; i++)
		setPWM(i, 0, 0);
	setPWM(apwm, 4096, 0);
	setPWM(bpwm, 4096, 0);
	setPWM(cpwm, 4096, 0);
	setPWM(dpwm, 4096, 0);
}

void MotorShieldV2::setPWMFreq(float freq)
{
	float prescaleval = 25000000;
	prescaleval /= 4096;
	prescaleval /= freq;
	prescaleval -= 1;
	uint8_t prescale = floor(prescaleval + 0.5);
	uint8_t oldmode = read8(PCA9685_MODE1);
	uint8_t newmode = (oldmode&0x7F) | 0x10;	// sleep

	write8(PCA9685_MODE1, newmode);				// go to sleep
	write8(PCA9685_PRESCALE, prescale);			// set the prescaler
	write8(PCA9685_MODE1, oldmode);
	delay(5);
	write8(PCA9685_MODE1, oldmode | 0xa1);		//  This sets the MODE1 register to turn on auto increment
}

void MotorShieldV2::setPWM(uint8_t num, uint16_t on, uint16_t off)
{
	Wire.beginTransmission(m_i2caddr);
	Wire.write(0x6+4*num);
	Wire.write(on);
	Wire.write(on>>8);
	Wire.write(off);
	Wire.write(off>>8);
	Wire.endTransmission();
}

uint8_t MotorShieldV2::read8(uint8_t addr)
{
	Wire.beginTransmission(m_i2caddr);
	Wire.write(addr);
	Wire.endTransmission();
	Wire.requestFrom(static_cast<uint8_t>(m_i2caddr), static_cast<uint8_t>(1));
	return Wire.read();
}

void MotorShieldV2::write8(uint8_t addr, uint8_t d)
{
	Wire.beginTransmission(m_i2caddr);
	Wire.write(addr);
	Wire.write(d);
	Wire.endTransmission();
}

void Stepper::set(uint8_t pin, bool x)
{
	m_shield->setPWM(pin, x ? 4096 : 0, 0);
}

void Stepper::setPWM(uint8_t pin, uint16_t x)
{
	if (x > 4095)
		m_shield->setPWM(pin, 4096, 0);
	else
		m_shield->setPWM(pin, 0, x);
}  

Stepper::Stepper(MotorShieldV2 *shield, uint8_t i): m_shield(shield), m_i(0)
{
	m_a = i ? ain1 : ain2;
	m_b = i ? bin1 : bin2;
	m_c = i ? cin1 : cin2;
	m_d = i ? din1 : din2;

	set(m_a, false);
	set(m_b, false);
	set(m_c, false);
	set(m_d, false);
}

void Stepper::step()
{
	switch (m_i)
	{
		case 0: set(m_d, false); set(m_a, true); break;
		case 1: set(m_a, false); set(m_b, true); break;
		case 2: set(m_b, false); set(m_c, true); break;
		case 3: set(m_c, false); set(m_d, true); break;
	}

	m_i++;

	if (m_i > 3)
		m_i = 0;
}

