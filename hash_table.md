# 散列表

如果所有的键都是小整数，我们可以使用一个数组来实现无序的符号表，将键作为数组的索引而数组中键 i 存储的就是它对应的值。这样我们就可以使用O(1)的复杂度快速访问任意键的值。哈希表的原理就是 **使用算术运算将键转化为数组的索引来访问数组中的键值对**。

理想情况下用散列函数会将不同的键转化为不同的索引值，当多个键散列到相同的索引值的时候就出现了 **哈希冲突**。解决 hash 冲突的两种方法是 *拉链法和线性探测法*。

散列表是 *在时间和空间上作出权衡* 的经典案例。如果没有内存限制，我们可以直接将键（可能是一个超大的数组）作为数组的索引，那么所有的查找操作都只需要访问一次内存。另一方面如果没有时间限制，我们可以使用无序数组进行顺序查找，这样就需要很少的内存。散列表使用了适度的空间和时间在两个极端之间取得了一种平衡。事实上不必要重写代码，只需要调整散列算法的参数就可以在空间和时间两者之间做出取舍。可以使用*概率论*的经典结论来帮助我们选择适当的参数。

## 散列函数

假设我们有一个保存 M 个键值对的数组，散列函数应该是一个容易计算并且均匀分布所有的键通过 hash 计算之后能够均匀分布到 [0,M-1]。

java 中的 HashMap 中使用长度为 2 的整数次幂的数组长度。这样做的好处是：计算一个元素的位置更快，使用位运算就可以了。相比取模，尤其是对素数取模的运算，要快很多。但是缺点是：容易哈希冲突。因此，有一种攻击方式叫做[哈希碰撞攻击](https://www.jianshu.com/p/5b99ae1ba9ce)

为了解决这个问题，HashMap 在哈希函数上做文章。HashMap 不是直接使用用户传来的数据的 hashCode 值直接 % length，而是对用户传来的数据的 hashCode 再进行一次哈希运算(这称为抖动函数)，以求得到的哈希值分布尽量平均。为此，HashMap 中有一个 hash(key) 的方法。

```java
/**
 * Computes key.hashCode() and spreads (XORs) higher bits of hash
 * to lower.  Because the table uses power-of-two masking, sets of
 * hashes that vary only in bits above the current mask will
 * always collide. (Among known examples are sets of Float keys
 * holding consecutive whole numbers in small tables.)  So we
 * apply a transform that spreads the impact of higher bits
 * downward. There is a tradeoff between speed, utility, and
 * quality of bit-spreading. Because many common sets of hashes
 * are already reasonably distributed (so don't benefit from
 * spreading), and because we use trees to handle large sets of
 * collisions in bins, we just XOR some shifted bits in the
 * cheapest possible way to reduce systematic lossage, as well as
 * to incorporate impact of the highest bits that would otherwise
 * never be used in index calculations because of table bounds.
 */
static final int hash(Object key) {
    int h;
    return (key == null) ? 0 : (h = key.hashCode()) ^ (h >>> 16);
}
```

### 整数

> 32 位整数的最大值 2147483647 是素数。

除留余数法。选择大小为**素数M**的数组，对于任意正整数k，计算`k % M`能有效将键分布在[0,M-1]。如果 M 不是素数我们可能无法均匀分布散列值。

![除留余数法](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/hash_method_mod.png)

上面的图中，由于键的后两位集中在 20，因此采用散列值 100 将会使得散列后的索引集中在 20 以内。选用素数将会使得散列的效果更好。与之相似，互联网中使用的 IP 地址也不是随机的，如果我们想要使用除留余数法将其散列就需要使用素数（特别低，这不是 2 的幂）大小的数组。

[哈希表中模的素数](https://planetmath.org/goodhashtableprimes)

### 浮点数

如果键是(0,1)之间的实数，可以将其乘以M并四舍五入得到0~M-1之间的索引值，尽管很容易理解，但是它的缺陷也是显而易见的：**键的高位起的作用更大，最低位对散列的结果没什么影响**。所以**对于浮点数，就是将键表示为二进制数然后再使用除留余数法**。

### 字符串

```js
// Horner方法
let hash = 0;
for(let i = 0;i < s.length;i++) {
  hash = (R * hash + s.charCodeAt(i)) % M; // 每次取模是为了防止溢出
}
```

```java
public int hashCode() {
  int h = hash;
  if (h == 0 && value.length > 0) {
    char val[] = value;

    for (int i = 0; i < value.length; i++) {
      h = 31 * h + val[i];
    }
    hash = h;
  }
  return h;
}

// Integer.MIN_VALUE的hashCode也是其自身
// java中的字符串居然后hashCode为负数的情况 = -2^31
// "polygenelubricants".hashCode() == -2147483648 
```

java 中的 String#hashCode 采用了素数31，并且使用了缓存（注意缓存一般只用在不可变对象上，用临时变量 h 的原因是防止多线程情况下直接修改 hash 可能导致错误的结果）。

### 组合键

可以采用和处理字符串一样的逻辑。假设我们的键是 Date，包含 3 个域 int day,int month,int year，就可以这样计算 hash：

```javascript
let hash = (((day * R + month) % M) * R + year) % M;
```

幸运的是 java 语言中常用的类都提供了 hashCode 方法，ide 可以帮助我们生成 hashCode。

## 拉链法

处理多个散列值相同的一种方法。将大小为M的数组中的每个元素指向一条无序链表，链表中的每个节点都存储了散列值为该元素的索引的键值对。这个方法的基本思路是选择足够大的 M，使得所有的链表尽可能短以保证查询的高效。查找分为2步：首先根据散列值找到对应的链表，然后沿着链表顺序查找相应的键。

![基于拉链法的散列表](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/hash_table_zip.png)

用M个链表保存N个键，则链表的平均长度时N/M。

```js
class SeparateChainingHashST {
  constructor(M = 977) {
    this.N = 0; // 键值对总数
    this.M = M; // 散列表大小（存放散列索引的数组大小）
    this.st = new Array(M).map(x => new SequentialSearchST()); // 存放链表对象的数组
  }

  _hash(key) {
    // 31个1，去掉符号位(最高位的结果肯定是0)
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  get(key) {
    return this.st[this._hash(key)].get(key);
  }

  put(key, value) {
    this.st[this._hash(key)].put(key, value);
  }
}
```

上面的代码复用了基于链表的顺序查找表：

```js
class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

// 符号表的实现：顺序查找表
class SequentialSearchST {

  constructor() {
    this.head = null;
    this.sz = 0;
  }

  put(key, value) {
    if (!this.head) {
      this.head = new Node(key, value);
      this.sz++;
    } else {
      // 向后扫描，如果找到相同的元素就将key替换为value
      let cur = this.head;
      while (cur) {
        if (cur.key === key) {
          cur.value = value;
          return;
        }
        cur = cur.next;
      }
      // 遍历完链表，没有找到，将新节点插入头部
      this.head = new Node(key, value, this.head);
      this.sz++;
    }
  }

  get(key) {
    if (!this.head) return null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return cur.value;
      }
      cur = cur.next;
    }
    return null;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  * keys() {
    if (!this.head) return;
    for (let cur = this.head; cur; cur = cur.next) {
      yield cur.key;
    }
  }

  delete(key) {
    // 找到待删除的节点的父节点
    const dummyHead = new Node(null, null, this.head);
    let parent = dummyHead;
    while (parent) {
      if (parent.next && parent.next.key === key) {
        break;
      }
      parent = parent.next;
    }
    if (!parent) {
      throw new Error(`no such key ${key}`);
    }
    parent.next = parent.next.next;
    this.head = dummyHead.next;
    dummyHead.next = null;
    this.sz--;
  }

  delete2(key) {
    // 不用虚拟头结点的删除
    let parent = null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        break;
      }
      parent = cur;
      cur = cur.next;
    }
    if (!cur) {
      throw new Error(`no such key ${key}`);
    }
    this.sz--;
    if (!parent) {
      this.head = cur.next;
    } else {
      parent.next = cur.next;
    }
  }
}
```

默认的构造函数会创建 977 条链表，因此对于比较大的符号表来说，这种实现比 SequentialSearchST 大约快 1000倍。更可靠的方案是动态调整链表数组的大小，这样无论符号表中有多少键值对都能保证链表较短。

> 在一张含有M条链表和N个键的散列表中，未命中查找和插入所需要的比较次数约为N/M。换而言之基于拉链法的散列表相比基于链表的顺序查找表性能提升了M倍。

基于拉链法的散列表实现简单，在键的顺序无关紧要的时候它可能是最快的（也是使用最广泛的）的符号表实现。

> java中的HashMap在java8之前，每个位置对应一条链表，java8之后，当哈希冲突达到一定程度（链表长度大于8）会转换为红黑树（红黑树是有序的要求元素有比较性，所以没有比较性的元素也不会转化为红黑树）。

## 线性探测法

使用大小为M的数组表示N个键值对（M > N）。这种方法 **依靠数组中的空位来解决哈希冲突**。基于这种策略的所有方法被统称为 ***开放地址散列表***。

开放地址散列表中最简单的实现是 ***线性探测法***。当碰撞发生时，直接检查散列表的下一个位置（将索引值加1）。这种线性探测可能产生3种结果：

1. 命中。该位置的键和被查找的键相同
2. 未命中。键为空（该位置没有键）
3. 继续查找。该位置的键和被查找的键不同

用散列函数找到键在数组中的索引，检查其中的键和被查找的键是否相同。如果不同则继续查找（将索引增大，到达数组末尾的时候折回到开头），直到找到该键或者找到一个空元素。检查一个数组位置是否含有被查找的键的操作的过程称为**探测**。在这里探测等价于***比较***和***测试键是否为空***。

开放地址类散列表的核心思想是：与其将内存作为链表，还不如将它们作为在散列表中的空元素，这些空元素可以作为查找结束的标志。

![基于线性探测法的散列表](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/hash_table_linear-probing.png)

和拉链法一样，开放地址类的散列表的性能也依赖于`a = N / M`的比值。a被称为散列表的*使用率*。基于拉链法的散列表a是每条链表的长度，一般都大于1；对于基于线性探测的散列表，a是表中已经占用空间的比例，不允许出现a为1的情况，因为此时的未命中查询会导致死循环。为了保证性能（散列表快满的时候探测的次数越来越大，当使用率小于1/2的时候预计探测次数只在1.5和2.5之间），需要动态调整数组保持a∈[1/8,1/2]之间。

java 中的 TreadLocalMap 就是通过开放地址法来解决哈希冲突的。散列表中的数据都存储在数组中，可以有效利用 CPU 缓存。而且这种方法实现的散列表序列化起来比较简单（不像链表法有指针）。缺点是：删除数据的时候比较麻烦，需要特殊标记已经删除的数据。所有的数据存储在一个数组中，比起链表法，冲突的代价更高。所以使用开放地址法的时候装载因子不能太大，这也就导致这种方法比起链表法需要更多的内存空间。—— 数据量小、装载因子小的时候使用开放地址法。

java 中的 LinkedHashMap 底层的实现是双向链表 + 哈希表。可以保持插入序和访问序（手动开启），它本身就是一个 LRU 缓存淘汰策略的缓存系统。它其中的 Linked 实际上指的是双向链表而*不是使用链表来解决哈希冲突*。

### 调整数组大小

可以通过调整数组的大小来保证散列表的使用率永远不会超过1/2。put()方法的第一句会调用resize()来保证散列表最多为半满状态。

```java
if( n >= m/2 )   resize( 2 * m );
```

当然在散列表过大时，我们在delete()时也需要

```javat
if ( n > 0 && n <= m/8 ) resize( m / 2 );
```

### 键簇

线性探索的平均成本取决于元素在插入数组后聚集成的一组连续的条目，也叫**键簇**。例如，在示例中插入键C会产生一个长度为3的键簇（A C S）。这意味着插入H需要探测4次，因为H的散列值为该键簇的第一个位置，显然，短小的键簇才能保证较高的效率。随着插入的键越来越多，这个要求很难满足，较长的键簇会越来越多。另外，因为数组的每个位置都有相同的可能性被插入一个新键，长键簇更长的可能性比短键簇更大，因为新键的散列值无论落在簇中的任何位置都会使簇的长度加1（甚至更多，如果这个簇和相邻的簇之间只有一个空元素相隔的话）

### 删除操作

基于线性探测的散列表中删除一个键 **直接将该键所在的位置设置为null是不行的**。因为它将导致所有存在这个位置后面的元素都返空（严格来说是该元素后面的元素可能和被删除的元素有相同的hash值，但是因为hash冲突被向后移动了的这种元素）。我们需要将簇中被删除键右侧的所有键重新插入到散列表。

```javascript
module.exports = class LinearProbingHashST {
  constructor(M = 16) {
    this.N = 0; // 符号表中键值对的总数
    this.M = M; // 线性探测表的大小
    this.keys = new Array(M); // 键
    this.values = new Array(M); // 值
  }

  _resize(cap) {
    const t = new LinearProbingHashST(cap);
    for (let i = 0; i < this.M; i++) {
      if (this.keys[i]) {
        t.put(this.keys[i], this.values[i]);
      }
    }
    this.keys = t.keys;
    this.values = t.values;
    this.M = t.M;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    if (this.N >= this.M / 2) this._resize(2 * this.M);
    let i = this._hash(key);
    for (; this.keys[i]; i = (i + 1) % this.M) {
      if (this.keys[i] === key) {
        this.values[i] = value;
        return;
      }
    }
    this.keys[i] = key;
    this.values[i] = value;
    this.N++;
  }

  get(key) {
    for (let i = this._hash(key); this.keys[i]; i = (i + 1) % this.M) {
      if (this.keys[i] === key) {
        return this.values[i];
      }
    }
    return null;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  delete(key) {
    if (!this.contains(key)) return;
    let i = this._hash(key);
    while (key !== this.keys[i]) {
      i = (i + 1) % this.M;
    }
    this.keys[i] = null;
    this.values[i] = null;
    i = (i + 1) % this.M;
    while (this.keys[i]) {
      const keyToRedo = this.keys[i];
      const valueToRedo = this.values[i];
      this.keys[i] = null;
      this.values[i] = null;
      this.N--;
      this.put(keyToRedo, valueToRedo);
      i = (i + 1) % this.M;
    }
    const N = --this.N;
    if (N > 0 && N === parseInt(N / 8)) {
      this._resize(parseInt(N / 2));
    }
  }
};
```

## 使用散列表存储稀疏向量

对于N*N的矩阵和大小为N的向量进行相乘，时间复杂度和N^2成正比。代码实现如下:

```java
// ...
int[][] matrix = new int[N][N];  // 矩阵
int[] x = new int[x]; // 向量
int[] ret = new int[x]; // 矩阵和向量乘法的结果为一个N*1的矩阵

// ...
// 初始化向量和矩阵
for(int i = 0;i < N;i++) {
  sum = 0;
  for(int j = 0;j < N;j++) {
    sum += matrix[i][j] * x[j]
  }
  ret[i] = sum;
}
```

在实际应用中，N往往非常巨大。幸好，我们使用的矩阵多数是稀疏的（大多数都是0），用HashMap就可以表示系数矩阵：

```java
public class SparseVector {
  private HashMap <Integer,Integer> st = new HashMap()<>;
  public void put(int index,int value){
    st.put(index,value);
  }
  public int get(index){
    if(!st.contains(index)){
      return 0;
    }
    return st.get(index);
  }
  public int dot(int[] that){
    int sum  = 0;
    for(int key : st.keySet()) {
      sum += that[i] * this.get(i);
    }
    return sum;
  }
}
```

以上符号表的用例实现了稀疏向量的主要功能并高效完成了点乘操作。将一个 向量中的每一项和两一个向量中的对应项相乘并将结果相加，所需要的乘法操作数量等于稀疏稀向量中农的非零项的数目。

![稀疏矩阵的表示.png](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E7%A8%80%E7%96%8F%E7%9F%A9%E9%98%B5%E7%9A%84%E8%A1%A8%E7%A4%BA.png)

这里不再使用`a[i][j]`来访问矩阵中的第i行和第j列的元素，而是使用`a[i].put(j,val)`来表示矩阵中的值并使用`a[i].get(j)`来获取它。用这种方式实现的矩阵和向量大的乘法比数组表示法的实现更简单（也更能清晰描述乘法的过程）。更重要的是，它所需要的时间仅和N加上矩阵中的非零元素的数量成正比。
