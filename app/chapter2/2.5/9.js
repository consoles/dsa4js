class DJIA {
  constructor(date, value) {
    this.date = date;
    this.value = value;
  }
  compareTo(other) {
    return this.value - other.value;
  }
}
