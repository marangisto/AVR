#include <Wire.h>

#include "MotorShield.h"

static Stepper *gM0 = 0;
static Stepper *gM1 = 0;

void setup()
{
  Serial.begin(9600);

  MotorShieldV2 *motorShield = new MotorShieldV2();

  gM0 = new Stepper(motorShield, 0);
  gM1 = new Stepper(motorShield, 1);
}

void loop()
{
	gM0->step();
	gM1->step();
  
	delay(100);
}

