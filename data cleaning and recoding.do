-----------------------------------------------------------------------------------
***********************************************************************************
/* Social Determinants of Health and Depressive Symptoms among 142,600 Older Adults: Findings from Six Cohort Studies across 33 Countries */
***********************************************************************************

/* The following code is about cleaning and recoding of the concerned variables, using the example of HRS wave 10-13 */



***merge harmonized HRS and RAND HRS***
set maxvar 35000
use "H_HRS_c.dta", clear

merge 1:1 hhidpn using "D:\SCI\Database\1. HRS (Y)\2. RankHRS\randhrs1992_2020v1_STATA\randhrs1992_2020v1.dta", force 
drop if _merge==1   //-1 (42,406)
drop _merge

gen n=_n
save "H_HRS_c.dta", replace


***define variable wave***
**wave 10**
sum h10hhid    //一共22,034人
gen wave10=0
replace wave10=1 if h10hhid != .
tab wave10

**wave 11**
sum h11hhid    //一共20,554人
gen wave11=0
replace wave11=1 if h11hhid != .
tab wave11

**wave 12**
sum h12hhid    //一共18,747人
gen wave12=0
replace wave12=1 if h12hhid != .
tab wave12

**wave 13**
sum h13hhid    //一共20,912人
gen wave13=0
replace wave13=1 if h13hhid != .
tab wave13

save "H_HRS_c.dta", replace



**********extracting and recoding concerned variables**********

********wave 10********
set maxvar 35000
use "H_HRS_c.dta", clear

keep if wave10==1    //-20,372
keep n ragender r10agey_e raeducl r10work r10lbrf r10mstat h10atotn r10peninc h10lvwith h10child r10smokef r10smokev r10drinkd r10hibpe r10diabe r10cancre r10lunge r10hearte r10stroke r10arthre r10walkra r10dressa r10batha r10eata r10beda r10toilta r10adl6a r10mapa r10phonea r10moneya r10medsa r10shopa r10mealsa r10iadl5a r10imrc r10dlrc r10orient r10higov r10prpcnt r10covr h10ahous h10afhous r10cesd r10depres r10effort r10sleepr r10whappy r10flone r10fsad r10going r10enlife
save "data_wave10", replace


use "data_wave10", clear    //n=22,034

label variable n "个人编码"
label variable ragender "性别"
label variable r10agey_e "年龄"
label variable raeducl"教育"
label variable r10work "是否最近为了赚钱而工作 (yes/no)"
label variable r10lbrf "劳动状态"
label variable r10mstat "婚姻状态"
label variable h10atotn "家庭收入水平"
label variable r10peninc "是否有养老金收入"
label variable h10child "是否有孩子"
label variable h10lvwith "和谁一起住"
label variable r10smokef "抽烟-一天抽的烟数"
label variable r10smokev "抽烟-是否抽过烟"
label variable r10drinkd "喝酒-过去三个月每周喝几天"
label variable r10hibpe "是否曾患高血压"
label variable r10diabe "是否曾患糖尿病"
label variable r10cancre "是否曾患肿瘤或癌症"
label variable r10lunge "是否曾患肺相关疾病"
label variable r10hearte "是否曾患心脏相关疾病"
label variable r10stroke "是否曾中风"
label variable r10arthre "是否曾患关节炎或风湿病"
label variable r10walkra "BADL室内行走"
label variable r10dressa "BADL穿衣服"
label variable r10batha "BADL洗澡"
label variable r10eata "BADL吃饭"
label variable r10beda "BADL起卧床"
label variable r10toilta "BADL上厕所"
label variable r10adl6a "BADL-汇总"
label variable r10phonea "IADL使用地图"
label variable r10moneya "IADL理财"
label variable r10medsa "IADL吃药"
label variable r10shopa "IADL购物"
label variable r10mealsa "IADL准备热食"
label variable r10iadl5a "IADL-汇总"
label variable r10imrc "认知：快速回忆"
label variable r10dlrc "认知：延迟回忆"
label variable r10orient "认知：日期报告"
label variable r10higov "医保：政府医保"
label variable r10prpcnt "医保：私人医保"
label variable r10covr "医保：来自工作的医保"
label variable h10ahous "是否有住房"
label variable h10afhous "是否有住房"
label variable r10cesd "抑郁得分 (8项是否)"

*0.id
sum n 
gen id=n
label variable id "个人编码"
sum id

*1.sex
tab ragender
gen sex=0
label variable sex "性别"
label define sex 1 "男" 2 "女"
label values sex "sex"
replace sex=1 if ragender==1
replace sex=2 if ragender==2
tab sex

*2.age
sum r10agey_e
gen age=r10agey_e
label variable age "年龄"
sum age

*3.education
tab raeducl
gen edu=raeducl
label variable edu "教育"
label define edu 1 "less than upper secondary" 2 "upper secondary and vocational training" 3 "tertiary"
label values edu "edu"
tab edu

*4.employment status
tab r10lbrf
gen employ=0
label variable employ "工作状态"
label define employ 1 "employed or partly retired or retired" 2 "unemployed"
label values employ "employ"
replace employ=1 if r10lbrf==1 | r10lbrf==2 | r10lbrf==4 |r10lbrf==5
replace employ=2 if r10lbrf==3 | r10lbrf==6 | r10lbrf==7
tab employ

*5.marital status
tab r10mstat
gen marriage=0
label variable marriage "婚姻状态"
label define marriage 1 "married or living witha a partner" 2 "not married nor living with a partner"
label values marriage "marriage"
replace marriage=1 if r10mstat==1 | r10mstat==2 | r10mstat==3 
replace marriage=2 if r10mstat==4 | r10mstat==5 | r10mstat==7 | r10mstat==8
tab marriage

*6.household wealth
sum h10atotn
gen housefw=h10atotn
label variable housefw "家庭收入水平"

*7.pension
tab r10peninc
gen pension=0
label variable pension "养老金"
label define pension 1 "no" 2 "yes"
label values pension "pension"
replace pension=1 if r10peninc==0
replace pension=2 if r10peninc==1
tab pension

**7.6.whether having children
tab h10child
gen childh=0
label variable childh "是否有孩子"
label define childh 1 "no" 0 "yes"
label values childh "childh"
replace childh=1 if h10child==0 
replace childh=0 if h10child>0 & h10child<=11
tab childh

*8.smoke
tab r10smokev
gen smoke=0
label variable smoke "是否曾吸烟"
label define smoke 1 "no" 2 "yes"
label values smoke "smoke"
replace smoke=1 if r10smokev==0
replace smoke=2 if r10smokev==1
tab smoke

*9.drink
tab r10drinkd
gen drink=0
label variable drink "过去三个月是否喝酒"
label define drink 1 "no" 2 "yes"
label values drink "drink"
replace drink=1 if r10drinkd==0
replace drink=2 if r10drinkd==1 | r10drinkd==2 | r10drinkd==3 | r10drinkd==4 | r10drinkd==5 | r10drinkd==6 | r10drinkd==7 
tab drink

*10.chronic conditions: hypertension/cancer/stroke
tab r10hibpe
tab r10cancre
tab r10stroke
gen hyper=0
gen cancer=0
gen stroke=0
label variable hyper "是否曾患高血压"
label variable cancer "是否曾患癌症或肿瘤"
label variable stroke "是否曾患中风"
label define hyper 1 "no" 2 "yes"
label define cancer 1 "no" 2 "yes"
label define stroke 1 "no" 2 "yes"
label values hyper "hyper"
label values cancer "cancer"
label values stroke "stroke"

replace hyper=1 if r10hibpe==0
replace hyper=2 if r10hibpe==1
replace cancer=1 if r10cancre==0
replace cancer=2 if r10cancre==1
replace stroke=1 if r10stroke==0
replace stroke=2 if r10stroke==1
tab hyper
tab cancer 
tab stroke

*11.BADL
tab r10walkra
tab r10dressa
tab r10batha
tab r10eata
tab r10beda
tab r10toilta
tab r10adl6a

gen badl=0
label variable badl "基本日常活动是否有困难"
label define badl 1 "no" 2 "yes"
label values badl "badl"
replace badl=1 if r10adl6a==0
replace badl=2 if r10adl6a==1 | r10adl6a==2 | r10adl6a==3 | r10adl6a==4 | r10adl6a==5 | r10adl6a==6
tab badl

*12.IADL
tab r10phonea
tab r10moneya
tab r10medsa
tab r10shopa
tab r10mealsa
tab r10iadl5a

gen iadl=0
label variable iadl "基本日常活动是否有困难"
label define iadl 1 "no" 2 "yes"
label values iadl "iadl"
replace iadl=1 if r10iadl5a==0
replace iadl=2 if r10iadl5a==1 | r10iadl5a==2 | r10iadl5a==3 | r10iadl5a==4 | r10iadl5a==5
tab iadl

*13.cognition
tab r10imrc
tab r10dlrc
tab r10orient

gen cog_imm=r10imrc
gen cog_del=r10dlrc
gen cog_ori=r10orient
label variable cog_imm "快速回忆"
label variable cog_del "延迟回忆"
label variable cog_ori "日期报告"    //缺失6,003人
tab cog_imm
tab cog_del
tab cog_ori

*14.health insurance
tab r10higov
tab r10prpcnt
tab r10covr

gen insure=0
label variable insure "是否有医疗保险"
label define insure 1 "no" 2 "yes"
label values insure "insure"
replace insure=1 if r10higov==0 & r10prpcnt==0 & r10covr==0
replace insure=2 if r10higov==1 | (r10prpcnt==1|r10prpcnt==2|r10prpcnt==3|r10prpcnt==4|r10prpcnt==5|r10prpcnt==7|r10prpcnt==12) | r10covr==1
tab insure

*15.house ownership
tab h10afhous
gen ownerr=0
label variable ownerr "是否有住房"
label define ownerr 1 "no" 2 "yes"
label values ownerr "ownerr"
replace ownerr=1 if h10afhous==6
replace ownerr=2 if h10afhous==1 | h10afhous==2 | h10afhous==3 | h10afhous==5    //没有缺失
tab ownerr

*16.depressive symptoms
tab r10cesd
gen depress=r10cesd
label variable depress "抑郁得分（连续，0-8）"
tab depress

gen depress1=0
label variable depress1 "抑郁得分（分2类，>=3）"
label define depress1 1 "no" 2 "yes"
label values depress1 "depress1"
replace depress1=1 if depress==0 | depress==1 | depress==2 
replace depress1=2 if depress==3 | depress==4 | depress==5 | depress==6 | depress==7 | depress==8
tab depress1
gen cesd1=r10depres
gen cesd2=r10effort
gen cesd3=r10sleepr
gen cesd4=1-r10whappy
gen cesd5=r10flone
gen cesd6=r10fsad 
gen cesd7=r10going
gen cesd8=1-r10enlife

*17.wave
gen wave=10
label variable wave "期数"

save "data_wave10.dta", replace




********wave 11********
set maxvar 35000
use "H_HRS_c.dta", clear

keep if wave11==1    //-21,852
keep hhidpn n ragender r11agey_e raeducl r11work r11lbrf r11mstat h11atotn r11peninc h11lvwith h11child r11smokef r11smokev r11drinkd r11hibpe r11diabe r11cancre r11lunge r11hearte r11stroke r11arthre r11walkra r11dressa r11batha r11eata r11beda r11toilta r11adl6a r11mapa r11phonea r11moneya r11medsa r11shopa r11mealsa r11iadl5a r11imrc r11dlrc r11orient r11higov r11prpcnt r11covr h11ahous h11afhous r11cesd r11depres r11effort r11sleepr r11whappy r11flone r11fsad r11going r11enlife

save "data_wave11", replace


use "data_wave11", clear    //n=20,554

label variable n "个人编码"
label variable ragender "性别"
label variable r11agey_e "年龄"
label variable raeducl"教育"
label variable r11work "是否最近为了赚钱而工作 (yes/no)"
label variable r11lbrf "劳动状态"
label variable r11mstat "婚姻状态"
label variable h11atotn "家庭收入水平"
label variable r11peninc "是否有养老金收入"
label variable h11lvwith "和谁一起住"
label variable h11child "是否有孩子"
label variable r11smokef "抽烟-一天抽的烟数"
label variable r11smokev "抽烟-是否抽过烟"
label variable r11drinkd "喝酒-过去三个月每周喝几天"
label variable r11hibpe "是否曾患高血压"
label variable r11diabe "是否曾患糖尿病"
label variable r11cancre "是否曾患肿瘤或癌症"
label variable r11lunge "是否曾患肺相关疾病"
label variable r11hearte "是否曾患心脏相关疾病"
label variable r11stroke "是否曾中风"
label variable r11arthre "是否曾患关节炎或风湿病"
label variable r11walkra "BADL室内行走"
label variable r11dressa "BADL穿衣服"
label variable r11batha "BADL洗澡"
label variable r11eata "BADL吃饭"
label variable r11beda "BADL起卧床"
label variable r11toilta "BADL上厕所"
label variable r11adl6a "BADL-汇总"
label variable r11phonea "IADL使用地图"
label variable r11moneya "IADL理财"
label variable r11medsa "IADL吃药"
label variable r11shopa "IADL购物"
label variable r11mealsa "IADL准备热食"
label variable r11iadl5a "IADL-汇总"
label variable r11imrc "认知：快速回忆"
label variable r11dlrc "认知：延迟回忆"
label variable r11orient "认知：日期报告"
label variable r11higov "医保：政府医保"
label variable r11prpcnt "医保：私人医保"
label variable r11covr "医保：来自工作的医保"
label variable h11ahous "是否有住房"
label variable h11afhous "是否有住房"
label variable r11cesd "抑郁得分 (8项是否)"

*0.id
sum n 
gen id=n
label variable id "个人编码"
sum id

*1.sex
tab ragender
gen sex=0
label variable sex "性别"
label define sex 1 "男" 2 "女"
label values sex "sex"
replace sex=1 if ragender==1
replace sex=2 if ragender==2
tab sex

*2.age
sum r11agey_e
gen age=r11agey_e
label variable age "年龄"
sum age

*3.education
tab raeducl
gen edu=raeducl
label variable edu "教育"
label define edu 1 "less than upper secondary" 2 "upper secondary and vocational training" 3 "tertiary"
label values edu "edu"
tab edu

*4.employment status
tab r11lbrf
gen employ=0
label variable employ "工作状态"
label define employ 1 "employed or partly retired or retired" 2 "unemployed"
label values employ "employ"
replace employ=1 if r11lbrf==1 | r11lbrf==2 | r11lbrf==4 |r11lbrf==5
replace employ=2 if r11lbrf==3 | r11lbrf==6 | r11lbrf==7
tab employ

*5.marital status
tab r11mstat
gen marriage=0
label variable marriage "婚姻状态"
label define marriage 1 "married or living witha a partner" 2 "not married nor living with a partner"
label values marriage "marriage"
replace marriage=1 if r11mstat==1 | r11mstat==2 | r11mstat==3 
replace marriage=2 if r11mstat==4 | r11mstat==5 | r11mstat==7 | r11mstat==8
tab marriage

*6.household wealth
sum h11atotn
gen housefw=h11atotn
label variable housefw "家庭收入水平"

*7.pension
tab r11peninc
gen pension=0
label variable pension "养老金"
label define pension 1 "no" 2 "yes"
label values pension "pension"
replace pension=1 if r11peninc==0
replace pension=2 if r11peninc==1
tab pension

**7.6.子childh
tab h11child
gen childh=0
label variable childh "是否有孩子"
label define childh 1 "no" 0 "yes"
label values childh "childh"
replace childh=1 if h11child==0 
replace childh=0 if h11child>0 & h11child<=11
tab childh

*8.smoke
tab r11smokev
gen smoke=0
label variable smoke "是否曾吸烟"
label define smoke 1 "no" 2 "yes"
label values smoke "smoke"
replace smoke=1 if r11smokev==0
replace smoke=2 if r11smokev==1
tab smoke

*9.drink
tab r11drinkd
gen drink=0
label variable drink "过去三个月是否喝酒"
label define drink 1 "no" 2 "yes"
label values drink "drink"
replace drink=1 if r11drinkd==0
replace drink=2 if r11drinkd==1 | r11drinkd==2 | r11drinkd==3 | r11drinkd==4 | r11drinkd==5 | r11drinkd==6 | r11drinkd==7 
tab drink

*10.chronic conditions: hypertension/cancer/stroke
tab r11hibpe
tab r11cancre
tab r11stroke
gen hyper=0
gen cancer=0
gen stroke=0
label variable hyper "是否曾患高血压"
label variable cancer "是否曾患癌症或肿瘤"
label variable stroke "是否曾患中风"
label define hyper 1 "no" 2 "yes"
label define cancer 1 "no" 2 "yes"
label define stroke 1 "no" 2 "yes"
label values hyper "hyper"
label values cancer "cancer"
label values stroke "stroke"

replace hyper=1 if r11hibpe==0
replace hyper=2 if r11hibpe==1
replace cancer=1 if r11cancre==0
replace cancer=2 if r11cancre==1
replace stroke=1 if r11stroke==0
replace stroke=2 if r11stroke==1
tab hyper
tab cancer 
tab stroke

*11.BADL
tab r11walkra
tab r11dressa
tab r11batha
tab r11eata
tab r11beda
tab r11toilta
tab r11adl6a

gen badl=0
label variable badl "基本日常活动是否有困难"
label define badl 1 "no" 2 "yes"
label values badl "badl"
replace badl=1 if r11adl6a==0
replace badl=2 if r11adl6a==1 | r11adl6a==2 | r11adl6a==3 | r11adl6a==4 | r11adl6a==5 | r11adl6a==6
tab badl

*12.IADL
tab r11phonea
tab r11moneya
tab r11medsa
tab r11shopa
tab r11mealsa
tab r11iadl5a

gen iadl=0
label variable iadl "基本日常活动是否有困难"
label define iadl 1 "no" 2 "yes"
label values iadl "iadl"
replace iadl=1 if r11iadl5a==0
replace iadl=2 if r11iadl5a==1 | r11iadl5a==2 | r11iadl5a==3 | r11iadl5a==4 | r11iadl5a==5
tab iadl

*13.cognition
tab r11imrc
tab r11dlrc
tab r11orient

gen cog_imm=r11imrc
gen cog_del=r11dlrc
gen cog_ori=r11orient
label variable cog_imm "快速回忆"
label variable cog_del "延迟回忆"
label variable cog_ori "日期报告"    //缺失10,304人
tab cog_imm
tab cog_del
tab cog_ori

*14.health insurance
tab r11higov
tab r11prpcnt
tab r11covr

gen insure=0
label variable insure "是否有医疗保险"
label define insure 1 "no" 2 "yes"
label values insure "insure"
replace insure=1 if r11higov==0 & r11prpcnt==0 & r11covr==0
replace insure=2 if r11higov==1 | (r11prpcnt==1|r11prpcnt==2|r11prpcnt==3|r11prpcnt==4|r11prpcnt==5|r11prpcnt==6|r11prpcnt==11) | r11covr==1
tab insure

*15.house ownership
tab h11afhous
gen ownerr=0
label variable ownerr "是否有住房"
label define ownerr 1 "no" 2 "yes"
label values ownerr "ownerr"
replace ownerr=1 if h11afhous==6
replace ownerr=2 if h11afhous==1 | h11afhous==2 | h11afhous==3 | h11afhous==5    //没有缺失
tab ownerr

*16.depressive symptoms
tab r11cesd
gen depress=r11cesd
label variable depress "抑郁得分（连续，0-8）"
tab depress

gen depress1=0
label variable depress1 "抑郁得分（分2类，>=3）"
label define depress1 1 "no" 2 "yes"
label values depress1 "depress1"
replace depress1=1 if depress==0 | depress==1 | depress==2 
replace depress1=2 if depress==3 | depress==4 | depress==5 | depress==6 | depress==7 | depress==8
tab depress1

*17.wave
gen wave=11
label variable wave "期数"
gen cesd1=r11depres
gen cesd2=r11effort
gen cesd3=r11sleepr
gen cesd4=1-r11whappy
gen cesd5=r11flone
gen cesd6=r11fsad 
gen cesd7=r11going
gen cesd8=1-r11enlife


save "data_wave11.dta", replace




********wave 12********
set maxvar 35000
use "H_HRS_c.dta", clear

keep if wave12==1    //
keep hhidpn n ragender r12agey_e raeducl r12work r12lbrf r12mstat h12atotn r12peninc h12lvwith h12child r12smokef r12smokev r12drinkd r12hibpe r12diabe r12cancre r12lunge r12hearte r12stroke r12arthre r12walkra r12dressa r12batha r12eata r12beda r12toilta r12adl6a r12mapa r12phonea r12moneya r12medsa r12shopa r12mealsa r12iadl5a r12imrc r12dlrc r12orient r12higov r12prpcnt r12covr h12ahous h12afhous r12cesd r12depres r12effort r12sleepr r12whappy r12flone r12fsad r12going r12enlife
save "data_wave12", replace

use "data_wave12", clear    //n=18,747

label variable n "个人编码"
label variable ragender "性别"
label variable r12agey_e "年龄"
label variable raeducl"教育"
label variable r12work "是否最近为了赚钱而工作 (yes/no)"
label variable r12lbrf "劳动状态"
label variable r12mstat "婚姻状态"
label variable h12atotn "家庭收入水平"
label variable r12peninc "是否有养老金收入"
label variable h12lvwith "和谁一起住"
label variable h12child "是否有孩子"
label variable r12smokef "抽烟-一天抽的烟数"
label variable r12smokev "抽烟-是否抽过烟"
label variable r12drinkd "喝酒-过去三个月每周喝几天"
label variable r12hibpe "是否曾患高血压"
label variable r12diabe "是否曾患糖尿病"
label variable r12cancre "是否曾患肿瘤或癌症"
label variable r12lunge "是否曾患肺相关疾病"
label variable r12hearte "是否曾患心脏相关疾病"
label variable r12stroke "是否曾中风"
label variable r12arthre "是否曾患关节炎或风湿病"
label variable r12walkra "BADL室内行走"
label variable r12dressa "BADL穿衣服"
label variable r12batha "BADL洗澡"
label variable r12eata "BADL吃饭"
label variable r12beda "BADL起卧床"
label variable r12toilta "BADL上厕所"
label variable r12adl6a "BADL-汇总"
label variable r12phonea "IADL使用地图"
label variable r12moneya "IADL理财"
label variable r12medsa "IADL吃药"
label variable r12shopa "IADL购物"
label variable r12mealsa "IADL准备热食"
label variable r12iadl5a "IADL-汇总"
label variable r12imrc "认知：快速回忆"
label variable r12dlrc "认知：延迟回忆"
label variable r12orient "认知：日期报告"
label variable r12higov "医保：政府医保"
label variable r12prpcnt "医保：私人医保"
label variable r12covr "医保：来自工作的医保"
label variable h12ahous "是否有住房"
label variable h12afhous "是否有住房"
label variable r12cesd "抑郁得分 (8项是否)"

*0.id
sum n 
gen id=n
label variable id "个人编码"
sum id

*1.sex
tab ragender
gen sex=0
label variable sex "性别"
label define sex 1 "男" 2 "女"
label values sex "sex"
replace sex=1 if ragender==1
replace sex=2 if ragender==2
tab sex

*2.age
sum r12agey_e
gen age=r12agey_e
label variable age "年龄"
sum age

*3.education
tab raeducl
gen edu=raeducl
label variable edu "教育"
label define edu 1 "less than upper secondary" 2 "upper secondary and vocational training" 3 "tertiary"
label values edu "edu"
tab edu

*4.employment status
tab r12lbrf
gen employ=0
label variable employ "工作状态"
label define employ 1 "employed or partly retired or retired" 2 "unemployed"
label values employ "employ"
replace employ=1 if r12lbrf==1 | r12lbrf==2 | r12lbrf==4 |r12lbrf==5
replace employ=2 if r12lbrf==3 | r12lbrf==6 | r12lbrf==7
tab employ

*5.marital status
tab r12mstat
gen marriage=0
label variable marriage "婚姻状态"
label define marriage 1 "married or living witha a partner" 2 "not married nor living with a partner"
label values marriage "marriage"
replace marriage=1 if r12mstat==1 | r12mstat==2 | r12mstat==3 
replace marriage=2 if r12mstat==4 | r12mstat==5 | r12mstat==7 | r12mstat==8
tab marriage

*6.household wealth
sum h12atotn
gen housefw=h12atotn
label variable housefw "家庭收入水平"

*7.pension
tab r12peninc
gen pension=0
label variable pension "养老金"
label define pension 1 "no" 2 "yes"
label values pension "pension"
replace pension=1 if r12peninc==0
replace pension=2 if r12peninc==1
tab pension

**7.6.whether having children
tab h12child
gen childh=0
label variable childh "是否有孩子"
label define childh 1 "no" 0 "yes"
label values childh "childh"
replace childh=1 if h12child==0 
replace childh=0 if h12child>0 & h12child<=11
tab childh

*8.smoke
tab r12smokev
gen smoke=0
label variable smoke "是否曾吸烟"
label define smoke 1 "no" 2 "yes"
label values smoke "smoke"
replace smoke=1 if r12smokev==0
replace smoke=2 if r12smokev==1
tab smoke

*9.drink
tab r12drinkd
gen drink=0
label variable drink "过去三个月是否喝酒"
label define drink 1 "no" 2 "yes"
label values drink "drink"
replace drink=1 if r12drinkd==0
replace drink=2 if r12drinkd==1 | r12drinkd==2 | r12drinkd==3 | r12drinkd==4 | r12drinkd==5 | r12drinkd==6 | r12drinkd==7 
tab drink

*10.chronic conditions: hypertension/cancer/stroke
tab r12hibpe
tab r12cancre
tab r12stroke
gen hyper=0
gen cancer=0
gen stroke=0
label variable hyper "是否曾患高血压"
label variable cancer "是否曾患癌症或肿瘤"
label variable stroke "是否曾患中风"
label define hyper 1 "no" 2 "yes"
label define cancer 1 "no" 2 "yes"
label define stroke 1 "no" 2 "yes"
label values hyper "hyper"
label values cancer "cancer"
label values stroke "stroke"

replace hyper=1 if r12hibpe==0
replace hyper=2 if r12hibpe==1
replace cancer=1 if r12cancre==0
replace cancer=2 if r12cancre==1
replace stroke=1 if r12stroke==0
replace stroke=2 if r12stroke==1
tab hyper
tab cancer 
tab stroke

*11.BADL
tab r12walkra
tab r12dressa
tab r12batha
tab r12eata
tab r12beda
tab r12toilta
tab r12adl6a

gen badl=0
label variable badl "基本日常活动是否有困难"
label define badl 1 "no" 2 "yes"
label values badl "badl"
replace badl=1 if r12adl6a==0
replace badl=2 if r12adl6a==1 | r12adl6a==2 | r12adl6a==3 | r12adl6a==4 | r12adl6a==5 | r12adl6a==6
tab badl

*12.IADL
tab r12phonea
tab r12moneya
tab r12medsa
tab r12shopa
tab r12mealsa
tab r12iadl5a

gen iadl=0
label variable iadl "基本日常活动是否有困难"
label define iadl 1 "no" 2 "yes"
label values iadl "iadl"
replace iadl=1 if r12iadl5a==0
replace iadl=2 if r12iadl5a==1 | r12iadl5a==2 | r12iadl5a==3 | r12iadl5a==4 | r12iadl5a==5
tab iadl

*13.cognition
tab r12imrc
tab r12dlrc
tab r12orient

gen cog_imm=r12imrc
gen cog_del=r12dlrc
gen cog_ori=r12orient
label variable cog_imm "快速回忆"
label variable cog_del "延迟回忆"
label variable cog_ori "日期报告"    //缺失9,055人
tab cog_imm
tab cog_del
tab cog_ori

*14.health insurance
tab r12higov
tab r12prpcnt
tab r12covr

gen insure=0
label variable insure "是否有医疗保险"
label define insure 1 "no" 2 "yes"
label values insure "insure"
replace insure=1 if r12higov==0 & r12prpcnt==0 & r12covr==0
replace insure=2 if r12higov==1 | (r12prpcnt==1|r12prpcnt==2|r12prpcnt==3|r12prpcnt==4|r12prpcnt==5|r12prpcnt==10|r12prpcnt==11|r12prpcnt==21) | r12covr==1
tab insure

*15.house ownership
tab h12afhous
gen ownerr=0
label variable ownerr "是否有住房"
label define ownerr 1 "no" 2 "yes"
label values ownerr "ownerr"
replace ownerr=1 if h12afhous==6
replace ownerr=2 if h12afhous==1 | h12afhous==2 | h12afhous==3 | h12afhous==5    //没有缺失
tab ownerr

*16.depressive symptoms
tab r12cesd
gen depress=r12cesd
label variable depress "抑郁得分（连续，0-8）"
tab depress

gen depress1=0
label variable depress1 "抑郁得分（分2类，>=3）"
label define depress1 1 "no" 2 "yes"
label values depress1 "depress1"
replace depress1=1 if depress==0 | depress==1 | depress==2 
replace depress1=2 if depress==3 | depress==4 | depress==5 | depress==6 | depress==7 | depress==8
tab depress1
gen cesd1=r12depres
gen cesd2=r12effort
gen cesd3=r12sleepr
gen cesd4=1-r12whappy
gen cesd5=r12flone
gen cesd6=r12fsad 
gen cesd7=r12going
gen cesd8=1-r12enlife

*17.wave
gen wave=12
label variable wave "期数"

save "data_wave12.dta", replace





********wave 13********
set maxvar 35000
use "H_HRS_c.dta", clear

keep if wave13==1    //-21,494
keep n ragender r13agey_e raeducl r13work r13lbrf r13mstat h13atotn r13peninc h13lvwith h13child r13smokef r13smokev r13drinkd r13hibpe r13diabe r13cancre r13lunge r13hearte r13stroke r13arthre r13walkra r13dressa r13batha r13eata r13beda r13toilta r13adl6a r13mapa r13phonea r13moneya r13medsa r13shopa r13mealsa r13iadl5a r13imrc r13dlrc r13orient r13higov r13prpcnt r13covr h13ahous h13afhous r13cesd r13depres r13effort r13sleepr r13whappy r13flone r13fsad r13going r13enlife
save "data_wave13", replace

use "data_wave13", clear    //n=20,912

label variable n "个人编码"
label variable ragender "性别"
label variable r13agey_e "年龄"
label variable raeducl"教育"
label variable r13work "是否最近为了赚钱而工作 (yes/no)"
label variable r13lbrf "劳动状态"
label variable r13mstat "婚姻状态"
label variable h13atotn "家庭收入水平"
label variable r13peninc "是否有养老金收入"
label variable h13lvwith "和谁一起住"
label variable h13child "是否有孩子"
label variable r13smokef "抽烟-一天抽的烟数"
label variable r13smokev "抽烟-是否抽过烟"
label variable r13drinkd "喝酒-过去三个月每周喝几天"
label variable r13hibpe "是否曾患高血压"
label variable r13diabe "是否曾患糖尿病"
label variable r13cancre "是否曾患肿瘤或癌症"
label variable r13lunge "是否曾患肺相关疾病"
label variable r13hearte "是否曾患心脏相关疾病"
label variable r13stroke "是否曾中风"
label variable r13arthre "是否曾患关节炎或风湿病"
label variable r13walkra "BADL室内行走"
label variable r13dressa "BADL穿衣服"
label variable r13batha "BADL洗澡"
label variable r13eata "BADL吃饭"
label variable r13beda "BADL起卧床"
label variable r13toilta "BADL上厕所"
label variable r13adl6a "BADL-汇总"
label variable r13phonea "IADL使用地图"
label variable r13moneya "IADL理财"
label variable r13medsa "IADL吃药"
label variable r13shopa "IADL购物"
label variable r13mealsa "IADL准备热食"
label variable r13iadl5a "IADL-汇总"
label variable r13imrc "认知：快速回忆"
label variable r13dlrc "认知：延迟回忆"
label variable r13orient "认知：日期报告"
label variable r13higov "医保：政府医保"
label variable r13prpcnt "医保：私人医保"
label variable r13covr "医保：来自工作的医保"
label variable h13ahous "是否有住房"
label variable h13afhous "是否有住房"
label variable r13cesd "抑郁得分 (8项是否)"

*0.id
sum n 
gen id=n
label variable id "个人编码"
sum id

*1.sex
tab ragender
gen sex=0
label variable sex "性别"
label define sex 1 "男" 2 "女"
label values sex "sex"
replace sex=1 if ragender==1
replace sex=2 if ragender==2
tab sex

*2.age
sum r13agey_e
gen age=r13agey_e
label variable age "年龄"
sum age

*3.education
tab raeducl
gen edu=raeducl
label variable edu "教育"
label define edu 1 "less than upper secondary" 2 "upper secondary and vocational training" 3 "tertiary"
label values edu "edu"
tab edu

*4.employment status
tab r13lbrf
gen employ=0
label variable employ "工作状态"
label define employ 1 "employed or partly retired or retired" 2 "unemployed"
label values employ "employ"
replace employ=1 if r13lbrf==1 | r13lbrf==2 | r13lbrf==4 |r13lbrf==5
replace employ=2 if r13lbrf==3 | r13lbrf==6 | r13lbrf==7
tab employ

*5.marital status
tab r13mstat
gen marriage=0
label variable marriage "婚姻状态"
label define marriage 1 "married or living witha a partner" 2 "not married nor living with a partner"
label values marriage "marriage"
replace marriage=1 if r13mstat==1 | r13mstat==2 | r13mstat==3 
replace marriage=2 if r13mstat==4 | r13mstat==5 | r13mstat==7 | r13mstat==8
tab marriage

*6.household wealth
sum h13atotn
gen housefw=h13atotn
label variable housefw "家庭收入水平"

*7.pension
tab r13peninc
gen pension=0
label variable pension "养老金"
label define pension 1 "no" 2 "yes"
label values pension "pension"
replace pension=1 if r13peninc==0
replace pension=2 if r13peninc==1
tab pension

**7.6.whether having children
tab h13child
gen childh=0
label variable childh "是否有孩子"
label define childh 1 "no" 0 "yes"
label values childh "childh"
replace childh=1 if h13child==0 
replace childh=0 if h13child>0 & h13child<=11
tab childh

*8.smoke
tab r13smokev
gen smoke=0
label variable smoke "是否曾吸烟"
label define smoke 1 "no" 2 "yes"
label values smoke "smoke"
replace smoke=1 if r13smokev==0
replace smoke=2 if r13smokev==1
tab smoke

*9.drink
tab r13drinkd
gen drink=0
label variable drink "过去三个月是否喝酒"
label define drink 1 "no" 2 "yes"
label values drink "drink"
replace drink=1 if r13drinkd==0
replace drink=2 if r13drinkd==1 | r13drinkd==2 | r13drinkd==3 | r13drinkd==4 | r13drinkd==5 | r13drinkd==6 | r13drinkd==7 
tab drink

*10.chronic conditions: hypertension/cancer/stroke
tab r13hibpe
tab r13cancre
tab r13stroke
gen hyper=0
gen cancer=0
gen stroke=0
label variable hyper "是否曾患高血压"
label variable cancer "是否曾患癌症或肿瘤"
label variable stroke "是否曾患中风"
label define hyper 1 "no" 2 "yes"
label define cancer 1 "no" 2 "yes"
label define stroke 1 "no" 2 "yes"
label values hyper "hyper"
label values cancer "cancer"
label values stroke "stroke"

replace hyper=1 if r13hibpe==0
replace hyper=2 if r13hibpe==1
replace cancer=1 if r13cancre==0
replace cancer=2 if r13cancre==1
replace stroke=1 if r13stroke==0
replace stroke=2 if r13stroke==1
tab hyper
tab cancer 
tab stroke

*11.BADL
tab r13walkra
tab r13dressa
tab r13batha
tab r13eata
tab r13beda
tab r13toilta
tab r13adl6a

gen badl=0
label variable badl "基本日常活动是否有困难"
label define badl 1 "no" 2 "yes"
label values badl "badl"
replace badl=1 if r13adl6a==0
replace badl=2 if r13adl6a==1 | r13adl6a==2 | r13adl6a==3 | r13adl6a==4 | r13adl6a==5 | r13adl6a==6
tab badl

*12.IADL
tab r13phonea
tab r13moneya
tab r13medsa
tab r13shopa
tab r13mealsa
tab r13iadl5a

gen iadl=0
label variable iadl "基本日常活动是否有困难"
label define iadl 1 "no" 2 "yes"
label values iadl "iadl"
replace iadl=1 if r13iadl5a==0
replace iadl=2 if r13iadl5a==1 | r13iadl5a==2 | r13iadl5a==3 | r13iadl5a==4 | r13iadl5a==5
tab iadl

*13.cognition
tab r13imrc
tab r13dlrc
tab r13orient

gen cog_imm=r13imrc
gen cog_del=r13dlrc
gen cog_ori=r13orient
label variable cog_imm "快速回忆"
label variable cog_del "延迟回忆"
label variable cog_ori "日期报告"    //缺失7,213人
tab cog_imm
tab cog_del
tab cog_ori

*14.health insurance 
tab r13higov
tab r13prpcnt
tab r13covr

gen insure=0
label variable insure "是否有医疗保险"
label define insure 1 "no" 2 "yes"
label values insure "insure"
replace insure=1 if r13higov==0 & r13prpcnt==0 & r13covr==0
replace insure=2 if r13higov==1 | (r13prpcnt==1|r13prpcnt==2|r13prpcnt==3|r13prpcnt==4|r13prpcnt==5|r13prpcnt==6|r13prpcnt==7|r13prpcnt==10|r13prpcnt==11) | r13covr==1
tab insure

*15.house ownership
tab h13afhous
gen ownerr=0
label variable ownerr "是否有住房"
label define ownerr 1 "no" 2 "yes"
label values ownerr "ownerr"
replace ownerr=1 if h13afhous==6
replace ownerr=2 if h13afhous==1 | h13afhous==2 | h13afhous==3 | h13afhous==5    //没有缺失
tab ownerr

*16.depressive symptoms
tab r13cesd
gen depress=r13cesd
label variable depress "抑郁得分（连续，0-8）"
tab depress

gen depress1=0
label variable depress1 "抑郁得分（分2类，>=3）"
label define depress1 1 "no" 2 "yes"
label values depress1 "depress1"
replace depress1=1 if depress==0 | depress==1 | depress==2 
replace depress1=2 if depress==3 | depress==4 | depress==5 | depress==6 | depress==7 | depress==8
tab depress1
gen cesd1=r13depres
gen cesd2=r13effort
gen cesd3=r13sleepr
gen cesd4=1-r13whappy
gen cesd5=r13flone
gen cesd6=r13fsad 
gen cesd7=r13going
gen cesd8=1-r13enlife

*17.wave
gen wave=13
label variable wave "期数"

save "data_wave13.dta", replace




**********merge the four waves of data**********
cd "D:\SCI\Database\1. HRS (Y)\1. harmonized data\HarmonizedHRSvC_STATA"
use "data_wave10", clear    //n=22,034  (用第10期数据)

append using "data_wave11"
append using "data_wave12"
append using "data_wave13"

save "data_wave10-13", replace




**********cleasing the concerned variables**********
use "data_wave10-13", clear    //n=82,247

*0.id
sum id

*1.sex
tab sex

*2.age
sum age
drop if age<60    //-27,657

*3.household wealth
sum housefw
sum housefw,d    //median: 45700
return list

gen housefw1=0
label variable housefw1 "家庭财产，二分类"
label define housefw1 1 "<50%" 2 ">=50%"
label values housefw1 "housefw1"
replace housefw1=1 if housefw<45700
replace housefw1=2 if housefw>=45700
tab housefw1

*4.education
tab edu
drop if edu==.m    //-14

*5.employment status
tab employ

*6.marital status
tab marriage
drop if marriage==0    //-56

*7.pension
tab pension
drop if pension==0    //-1,110

**7.6.whether having children
tab childh

*8.smoke
tab smoke
drop if smoke==0    //-372

*9.drink
tab drink
drop if drink==0    //-179

*10.chronic conditions: hypertension/cancer/stroke
tab hyper
tab cancer 
tab stroke

*11.badl
tab badl
drop if badl==0    //-5

*12.health insurance
tab insure
drop if insure==0    //-209

*13.house ownership
tab ownerr
drop if ownerr==0    //-231

*14.depressive symptoms
tab depress1
drop if depress1==0    //-3,625

*15.wave
tab wave


alpha cesd1 cesd2 cesd3 cesd4 cesd5 cesd6 cesd7 cesd8
save "data_wave10-13.dta", replace    //n=48,952



**********recoding the concerned variables**********
******1.education
tab edu
gen edu1=0
label variable edu1 "教育，二分类"
label define edu1 1 "less than upper secondary" 0 "upper secondary and vocational training or higher"
label values edu1 "edu1"
replace edu1=1 if edu==1
replace edu1=0 if edu==2 | edu==3
tab edu1

******2.employment status
tab employ
gen employ1=0
label variable employ1 "工作，二分类"
label define employ1 1 "unemployed" 0 "employed or partly retired or retired"
label values employ1 "employ1"
replace employ1=1 if employ==2
replace employ1=0 if employ==1
tab employ1

******3.marital status
tab marriage
gen marriage1=0
label variable marriage1 "婚姻，二分类"
label define marriage1 1 "not married nor living with a partner " 0 "married or living witha a partner"
label values marriage1 "marriage1"
replace marriage1=1 if marriage==2
replace marriage1=0 if marriage==1
tab marriage1

******4.household wealth
tab housefw1
gen housefw2=0
label variable housefw2 "家庭财产，二分类"
label define housefw2 1 "<50%" 0 ">=50%"
label values housefw2 "housefw2"
replace housefw2=1 if housefw1==1
replace housefw2=0 if housefw1==2
tab housefw2

******5.pension
tab pension
gen pension1=0
label variable pension1 "养老金，二分类"
label define pension1 1 "no" 0 "yes"
label values pension1 "pension1"
replace pension1=1 if pension==1
replace pension1=0 if pension==2
tab pension1

******6.5.whether having children
tab childh

******7.health insurance
tab insure
gen insure1=0
label variable insure1 "医保，二分类"
label define insure1 1 "no" 0 "yes"
label values insure1 "insure1"
replace insure1=1 if insure==1
replace insure1=0 if insure==2
tab insure1

******8.house ownership
tab owner
gen owner1=0
label variable owner1 "住房，二分类"
label define owner1 1 "no" 0 "yes"
label values owner1 "owner1"
replace owner1=1 if ownerr==1
replace owner1=0 if ownerr==2
tab owner1

******depressive symptoms
tab depress1
label drop depress1
label define depress1 1 "yes" 0 "no"
replace depress1=0 if depress1==1
replace depress1=1 if depress1==2
tab depress1

******chronic conditions
tab hyper
tab stroke
tab cancer
gen chronic=0
label variable chronic "慢性病，二分类"
label define chronic 0 "no" 1 "yes"
label values chronic "chronic"
replace chronic=1 if hyper==2 | stroke==2 | cancer==2
replace chronic=0 if hyper==1 & stroke==1 & cancer==1
tab chronic

****** cumulative social determinants of health 
gen sdoh=0
label variable sdoh "社会决定因素，连续"
label values sdoh "sdoh"
replace sdoh=edu1+marriage1+employ1+housefw2+pension1+childh+insure1+owner1
tab sdoh

gen sdoh11=sdoh
label variable sdoh11 "社会决定因素，6类"
label values sdoh11 "sdoh11"
replace sdoh11=7 if sdoh==8
tab sdoh11

gen sdoh1=sdoh
label variable sdoh1 "社会决定因素，6类"
label values sdoh1 "sdoh1"
replace sdoh1=6 if sdoh==7 | sdoh==8
tab sdoh1    //for analysis


