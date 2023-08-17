def gradeFromScore(score) {
    switch (score) {
        // 左右开区间
        case 90..100: return "A"
        // 左开右闭区间
        case 80..<90: return "B"
        case 70..<80: return "C"
        case 60..<70: return "D"
        case 0..<60: return "F"
        // 正则匹配
        case ~"[ABCDFabcdf]": return score.toUpperCase()
        default: throw IllegalArgumentException("Invalid score: $score")
    }
}

// A
// B
// D
// F
println gradeFromScore(92)
println gradeFromScore(85)
println gradeFromScore(65)
println gradeFromScore("f")
