// 保留目录下的pdf文件
// 场景：百度云下载的学习资料有音频、html和pdf格式，内容是一样的，保留pdf就行了

const fs = require('fs')
const path = require('path')

function deleteNotPdfFiles(dir) {
    if (!fs.existsSync(dir)) return console.error('dir',dir,'not exists!')
    const stat = fs.statSync(dir)
    if (stat.isDirectory()) {
        for(const filename of fs.readdirSync(dir)) {
            deleteNotPdfFiles(path.join(dir,filename))
        }
    } else {
        const {ext} = path.parse(dir)
        if (ext.toLowerCase() !== '.pdf') {
            console.warn('delete file',dir)
            fs.unlinkSync(dir)
        }
    }
}

const dir = 'F:\\BaiduNetdiskDownload\\101-后端技术面试38讲\\'
deleteNotPdfFiles(dir)