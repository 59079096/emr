# emr
基于HCView开发的电子病历程序，此代码仅做学习交流使用，不可用于商业目的，由此引发的后果请使用者承担，你可以加入QQ群 649023932 来获取更多的技术交流。

代码目录说明：
----CFControl：几个自己写的控件
----Client：客户端主程序代码
----ClientPlugin：客户端功能模块插件代码
-------InchDoctorStation：住院医生站模块代码
-------Login：登录模块代码
-------Template：模板制作模块代码
----Common：公共代码
--------diocp：DIOCP代码目录（此代码为第三方开源项目，见附录中的说明）
----PluginFramework：插件框架实现代码
----Update：客户端升级程序代码

编译步骤：
配置以下目录到开发环境中（在delphi中的菜单Tools-Options对话框里选中Library节点，右侧Library path添加），编译如果提示找不到Delphi自带的单元，则在工程属性中根据实际情况添加命名空间域Winapi、Vcl、Vcl.Imaging（Project-Options-Delphi Compiler右侧Unit scope names）
1.加入QQ群 649023932，到群共享里下载emr demo的数据库文件，并在sqlserver2008及以上中恢复，同时设置好访问emrDB的账号和密码
2.配置CFControl代码目录到开发环境
3.配置HCView代码目录到开发环境
4.配置Common代码目录到开发环境
5.配置diocp代码目录到开发环境
6.配置PluginFramework代码目录到开发环境
7.安装CFControl控件包
8.编译BLLServer里的emrBLLServer
9.运行emrBLLServer.exe，通过配置菜单中的连接，设置好步骤1中的数据库参数，然后在点击运行菜单，运行成功后，在标题栏有端口号。
10.编译ClientPlugin下各个插件，生成到ClientBin\plugin
11.编译Client，生成到ClientBin
12.运行编译好的exe，等待3秒会弹出提示连接服务端错误对话框，点击配置，配置9中服务端的IP地址和端口号

附录：
DIOCP-v5开源项目
地址
https://github.com/ymofen/diocp-v5
https://git.oschina.net/ymofen/diocp-v5.git
本群为技术讨论群，如果有技术问题的时候请大家停止吹水，贴图。
1.请大家尽量帮忙解答群友的问题。
2.本群优先解答diocp问题，请熟悉的朋友不吝赐教。
3.DIOCPDEMO问题请先看看DEMO说明。FAQ中的提及的文档。
4.请教技术问题请百度->再提问，多思考才会领悟深刻。
5.本群禁止讨论股票。
注意: 广告(招聘，买卖，交易)以后每周二和五可以发一次，平常发广告禁言一次，第二次T
最新版zip下载
https://codeload.github.com/ymofen/diocp-v5/zip/master
YxdIOCP开源地址(小白)
https://github.com/yangyxd/YxdIOCP