class XDomain {
  constructor(domain) {
    this.domains = domain.split('.').reverse();
    this.domain = domain;
  }

  compareTo(other) {
    const minLen = Math.min(this.domains.length, other.domains.length);
    for (let i = 0; i < minLen; i++) {
      if (this.domains[i] > other.domains[i]) {
        return 1;
      }
      if (this.domains[i] < other.domains[i]) {
        return -1;
      }
    }
    return this.domains.length - other.domains.length;
  }
}

const domains = ['google.com', 'jsj.hgnc.edu', 'hgnc.edu', 'apple.com'].map(x => new XDomain(x));
domains.sort((a, b) => a.compareTo(b));
debugger;
