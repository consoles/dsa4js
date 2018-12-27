// 1.2.11 根据Date的API实现一个SmartDate类型，在日期非法时抛出一个异常。
// 1.2.12 为SmartDate添加一个方法dayOfTheWeek()，为日期中每周的日返回Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday中的适当值。你可以假定时间是21世纪

const assert = require('assert');

class SmartDate {
    constructor(year, month, day) {
        assert(year >= 2000 && year < 1e4 && Number.isInteger(year), '年份不合法');
        assert(Number.isInteger(month) && month >= 1 && month <= 12, '月份不合法');
        const dayOfMonths = [0, 31, -1, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        const days = dayOfMonths[month];
        assert(days, '天数不合法');
        if (days === -1) {
            const dayOfFeb = SmartDate.isLeapYear(year) ? 29 : 28;
            assert.equal(day, dayOfFeb, '2月份天数不合法');
        }
        this.year = year;
        this.month = month;
        this.day = day;
    }

    static isLeapYear(year) {
        return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
    }

    dayOfTheWeek() {
        // 2000/1/1 -> Sat
        const weekdays = ['Sat', 'Sun', 'Mon', 'Tue', 'Wed', 'Thur', 'Fri'];
        // const dayOfMonths = [0, 31, -1, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        // // get offset days
        // let offsetDay = 0;
        // for (let i = 2000; i < this.year; i++) {
        //     offsetDay += SmartDate.isLeapYear(i) ? 366 : 365;
        // }
        // for (let i = 1; i < this.month; i++) {
        //     offsetDay += dayOfMonths[i];
        // }
        // offsetDay += this.day;
        // return weekdays[offsetDay % 7];

        // 使用蔡勒公式
        let year = this.year;
        let month = this.month;

        if (month === 1 || month === 2) {
            month += 12;
            year--;
        }

        const c = parseInt(year / 100);
        const y = year % 100;
        const m = month;
        const d = this.day;
        const w = parseInt(c / 4) - 2 * c + y + parseInt(y / 4) + parseInt(26 * (m + 1) / 10) + d - 1;
        const index = w < 0 ? (w + (parseInt(-w / 7) + 1) * 7) % 7 : w % 7;
        return ['Sun', 'Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat'][index];
    }
}

const date = new SmartDate(2018, 12, 27);
const ret = date.dayOfTheWeek();
debugger;