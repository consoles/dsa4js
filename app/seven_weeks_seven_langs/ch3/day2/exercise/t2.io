// 分母为 0 的情况下， 让 / 返回 0

(112 / 0) println // inf

orig := Number getSlot("/") // 原本除法的 Slot
Number / := method(num, if (num == 0, 0, orig num))

(113 / 0) println // 0
