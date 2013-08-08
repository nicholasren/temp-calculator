天气预报温度准确率计算
===
本软件通过分析从2009年到2012年对最高最低气温（72小时，48小时，24小时）的预报数据和实际最高最低气温分析，得出气温预报的月、季度准确率。


####环境：
1. scala
2. java

####运行：
在本目录下，windows用户请执行`run.bat`, linux用户请执行`run.sh`，会生成csv形式的报告。

例如，CCTV的2012年季度预报准确率如下：

|Year|Season|24h High| 24h Low|48h High| 48h Low| 72h High| 72h Low|
|----|------|--------|--------|--------|--------|---------|--------|
|2012| 01| 50.549454| 43.956043|52.747257|54.945057|49.45055 |54.945057|
|2012| 02| 58.24176 | 75.82418 |41.758244|67.03297 |43.956043|63.736267|
|2012| 03| 57.608692| 86.95652 |58.69565 |78.26087 |52.173912|76.08696|
|2012| 04| 35.869564| 73.91304 |36.95652 |64.13044 |36.95652 |65.21739|

