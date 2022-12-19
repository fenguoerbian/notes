
# 用CMH权重计算分层risk difference

咱们计算response rate的时候需要用MN的方法计算分层的risk difference，用CMH的方法计算p值。但是有些研究在计算分层risk difference的时候需要用CMH的权重。

- `ratediff_strata_mn.R`: R 代码，需要软件包`ratesci`。

- `ratediff_strata_mn.sas`: SAS代码，来自于127_Final_Paper_PDF.pdf，可能是收录于[2016的WUSS conference](https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf)。

- `scoreci.sas`: SAS代码，`ratesci`包作者提供的[sas code repo](https://github.com/PeteLaud/ratesci-sas)。

