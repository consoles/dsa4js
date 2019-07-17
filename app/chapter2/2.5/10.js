class Version {
  constructor(major, minor, patch) {
    this.major = major;
    this.minor = minor;
    this.patch = patch;
  }

  compareTo(other) {
    let diff = this.major - other.major;
    if (diff !== 0) return diff;
    diff = this.minor - other.minor;
    if (diff !== 0) return diff;
    return this.patch - other.patch;
  }
}

const v1 = new Version(115, 1, 1);
const v2 = new Version(115, 10, 1);
const ret = v1.compareTo(v2);
debugger;
