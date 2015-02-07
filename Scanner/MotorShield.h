#ifndef MOTOR_SHIELD_H
#define MOTOR_SHIELD_H 1

class MotorShieldV2 {
public:
	MotorShieldV2(uint8_t addr = 0x40);
	void reset(void);
	void setPWMFreq(float freq);
	void setPWM(uint8_t num, uint16_t on, uint16_t off);

private:
	uint8_t read8(uint8_t addr);
	void write8(uint8_t addr, uint8_t d);

	uint8_t m_i2caddr;
};

class Stepper
{
public:
	Stepper(MotorShieldV2 *shield, uint8_t i);
	void step();

private:
	void set(uint8_t pin, bool x);
	void setPWM(uint8_t pin, uint16_t x);

	MotorShieldV2	*m_shield;
	uint8_t			m_a, m_b, m_c, m_d;
	uint8_t			m_i;
};

#endif // MOTOR_SHIELD

