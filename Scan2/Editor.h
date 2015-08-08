#ifndef EDITOR_H
#define EDITOR_H

#include "A4988.h"

//template<class T> T min(const T& x, const T& y) { return x < y ? x : y; }
//template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

struct item_i
{
	virtual const char *name() const = 0;
	virtual const char *show(char *buf) const = 0;
	virtual void incr(bool fast = false) = 0;
	virtual void decr(bool fast = false) = 0;
};

template<class T>
struct item_t: public item_i
{
};

template<>
struct item_t<bool>: public item_i
{
	typedef bool ty;
	item_t(const char *s, const ty& x): m_s(s), m_x(x) {}
	virtual const char *name() const { return m_s; }
	virtual const char *show(char *buf) const { return m_x ? "true" : "false"; }
	virtual void incr(bool _ = false) { m_x = !m_x; }
	virtual void decr(bool _ = false) { m_x = !m_x; }
	ty& value() { return m_x; }

	const char *m_s;
	ty			m_x;
};

template<>
struct item_t<uint8_t>: public item_i
{
	typedef uint8_t ty;
	item_t(const char *s, const ty& x): m_s(s), m_x(x) {}
	virtual const char *name() const { return m_s; }
	virtual const char *show(char *buf) const { return utoa(m_x, buf, 10); }
	virtual void incr(bool fast = false) { m_x += fast ? 16 : 1; }
	virtual void decr(bool fast = false) { m_x -= fast ? 16 : 1; }
	ty& value() { return m_x; }

	const char *m_s;
	ty			m_x;
};

template<>
struct item_t<uint16_t>: public item_i
{
	typedef uint16_t ty;
	item_t(const char *s, const ty& x): m_s(s), m_x(x) {}
	virtual const char *name() const { return m_s; }
	virtual const char *show(char *buf) const { return utoa(m_x, buf, 10); }
	virtual void incr(bool fast = false) { m_x += fast ? 256 : 1; }
	virtual void decr(bool fast = false) { m_x -= fast ? 256 : 1; }
	ty& value() { return m_x; }

	const char *m_s;
	ty			m_x;
};

template<>
struct item_t<micro_step_t::e>: public item_i
{
	typedef micro_step_t::e ty;
	item_t(const char *s, const ty& x): m_s(s), m_x(x) {}
	virtual const char *name() const { return m_s; }
	virtual const char *show(char *buf) const { return micro_step_t::to_string(m_x); }
	virtual void incr(bool _ = false)
	{
		switch (m_x)
		{
			case micro_step_t::full_step: m_x = micro_step_t::half_step; break;
			case micro_step_t::half_step: m_x = micro_step_t::quarter_step; break;
			case micro_step_t::quarter_step: m_x = micro_step_t::eigth_step; break;
			case micro_step_t::eigth_step: m_x = micro_step_t::sixteenth_step; break;
			case micro_step_t::sixteenth_step: m_x = micro_step_t::full_step; break;
		}
	}
	virtual void decr(bool _ = false)
	{
		switch (m_x)
		{
			case micro_step_t::full_step: m_x = micro_step_t::sixteenth_step; break;
			case micro_step_t::half_step: m_x = micro_step_t::full_step; break;
			case micro_step_t::quarter_step: m_x = micro_step_t::half_step; break;
			case micro_step_t::eigth_step: m_x = micro_step_t::quarter_step; break;
			case micro_step_t::sixteenth_step: m_x = micro_step_t::eigth_step; break;
		}
	}
	ty& value() { return m_x; }

	const char *m_s;
	ty			m_x;
};

class editor_t: public item_i
{
public:
	editor_t(item_i **p, size_t n): m_p(p), m_n(n), m_i(0) {}
	virtual const char *name() const { return m_p[m_i]->name(); }
	virtual const char *show(char *buf) const { return m_p[m_i]->show(buf); }
	virtual void incr(bool fast = false) { m_p[m_i]->incr(fast); }
	virtual void decr(bool fast = false) { m_p[m_i]->decr(fast); }
	void next() { if (++m_i >= m_n) m_i = 0; }

private:
	item_i		**m_p;
	size_t		m_n;
	size_t		m_i;
};

#endif // EDITOR_H

