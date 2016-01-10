# Emacs 配置

配置主要参考 [prelude](https://github.com/bbatsov/prelude) 和 [oh-my-emacs](https://github.com/xiaohanyu/oh-my-emacs)，在了解他们的配置后，修改成我需要的配置。

在包管理方面 `el-get` 和 `package` 都试用过，最终还是选择了 `package`，毕竟天朝的网络对于 `github` 不太友好。

在 `core` 中存放的是我认为必须存在的配置，而 `modules` 中则是可选的，通过 `sample-module.el` 来控制模块是否加载。

## Depends

* cmake, libclang
>>For irony or ycmd, irony need run 'irony-install-server' at first.
