class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next;
  }
}

/**
 * 使用有序链表来实现有序符号表的API
 */
class OrderedSequentialSearchST {
  constructor() {
    this.sz = 0;
    this.head = null;
  }

  put(key, value) {
    if (!this.head) {
      this.head = new Node(key, value);
      this.sz++;
    } else {
      // 从前向后扫描并插入节点
      let parent = null;
      let cur = this.head;
      while (cur) {
        if (cur.key === key) {
          cur.value = value;
          return;
        }
        if (cur.key > key) {
          break;
        }
        parent = cur;
        cur = cur.next;
      }
      if (parent) {
        parent.next = new Node(key, value, cur);
      } else {
        this.head = new Node(key, value, this.head);
      }
      this.sz++;
    }
  }

  get(key) {
    if (this.isEmpty()) {
      return null;
    }
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return cur.value;
      }
      cur = cur.next;
    }
    return null;
  }

  delete(key) {
    if (this.isEmpty()) throw new Error('符号表为空');
    let cur = this.head;
    let parent = null;
    while (cur) {
      if (cur.key === key) {
        break;
      }
      parent = cur;
      cur = cur.next;
    }
    if (!cur) {
      throw new Error('符号表为空');
    }
    if (cur === this.head) {
      this.head = cur.next;
    } else {
      parent.next = cur.next;
    }
    this.sz--;
  }


  * keys() {
    if (this.isEmpty()) {
      return yield [];
    }
    let cur = this.head;
    while (cur) {
      yield cur.key;
      cur = cur.next;
    }
  }

  isEmpty() {
    return this.size === 0;
  }

  get size() {
    return this.sz;
  }

  minKey() {
    return this.head.key;
  }

  maxKey() {
    let cur = this.head;
    let maxKey = null;
    while (cur) {
      maxKey = cur.key;
      cur = cur.next;
    }
    return maxKey;
  }

  ceil(key) {
    if (this.isEmpty()) throw new Error('符号表为空');
    let cur = this.head;
    while (cur) {
      if (cur.key >= key) {
        return cur.key;
      }
      cur = cur.next;
    }
    return null;
  }

  floor(key) {
    if (this.isEmpty()) throw new Error('符号表为空');
    let cur = this.head;
    let v = null;
    while (cur) {
      if (cur.key <= key) {
        v = cur.key;
      } else {
        break;
      }
      cur =cur.next;
    }
    return v;
  }

  rank(key) {
    let i = 0;
    let cur = this.head;
    while (cur) {
      if (cur.key >= key) {
        return i;
      }
      cur = cur.next;
      i++;
    }
    return i;
  }

  select(index) {
    if (index < 0 || index >= this.size) throw new Error('select index error');
    let i = 0;
    let cur = this.head;
    while (cur) {
      if (i === index) {
        return cur.key;
      }
      i++;
      cur = cur.next;
    }
    return null;
  }
}

const keys = 'SEARCHEXAMPLE'.split('');
const st = new OrderedSequentialSearchST();
for (let i = 0; i < keys.length; i++) {
  st.put(keys[i], i);
}
debugger;
const setKeys = st.keys();
for (const key of setKeys) {
  console.log(key);
}

st.delete('A');
st.delete('L');
st.delete('X');

let value =st.get('S');
let key = st.minKey();
key = st.maxKey();
key = st.floor('D');
key = st.ceil('D');
let index = st.select(0);
let rank = st.rank('A');
