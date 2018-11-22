const path = require("path");

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

module.exports = {
    entry: resolve("XmasList.tests.fsproj"),
    outDir: resolve("output"),
    babel: {
        "plugins": ["@babel/plugin-transform-modules-commonjs"]
    },
    allFiles: false
}
