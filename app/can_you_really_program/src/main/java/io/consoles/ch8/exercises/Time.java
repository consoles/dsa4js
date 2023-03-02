package io.consoles.ch8.exercises;

/**
 * 不可变类，每个共有方法返回一个新的 Time 对象
 */
public class Time {

    private final int hours, minutes, seconds;

    public Time(int hours, int minutes, int seconds) {
        if (hours < 0 || hours > 23)
            throw new IllegalArgumentException("The range for hours is 0, 23.");
        if (minutes < 0 || minutes > 59)
            throw new IllegalArgumentException("The range for minutes is 0, 59.");
        if (seconds < 0 || seconds > 59)
            throw new IllegalArgumentException("The range for seconds is 0, 59.");
        this.hours = hours;
        this.minutes = minutes;
        this.seconds = seconds;
    }

    /**
     * 给这个时间增加一段时间，最大可达零点前一秒（23：59：59）。
     *
     * @param delta
     */
    public Time addNoWrapping(Time delta) {
        int s = seconds, m = minutes, h = hours;
        s += delta.seconds;
        if (s > 59) {
            s -= 60;
            m++;
        }
        m += delta.minutes;
        if (m > 59) {
            m -= 60;
            h++;
        }
        h += delta.hours;
        // overflow
        if (h > 23) {
            h = 23;
            m = 59;
            s = 59;
        }
        return new Time(h, m, s);
    }

    /**
     * 给这个时间增加一段时间，会突破零点。
     *
     * @param delta
     */
    public Time addAndWrapAround(Time delta) {
        int s = seconds, m = minutes, h = hours;
        s += delta.seconds;
        if (s > 59) {
            s -= 60;
            m++;
        }
        m += delta.minutes;
        if (m > 59) {
            m -= 60;
            h++;
        }
        h += delta.hours;
        // overflow
        if (h > 23) {
            h -= 24;
        }
        return new Time(h, m, s);
    }
}
