use fxhash::FxHashMap;
use once_cell::sync::Lazy;

static DEFAULT_GRP_SIZES: &[(&[u8], (u16, u16))] = &[
    (b"zerg\\avenger.grp", (48, 48)),
    (b"zerg\\zavbirth.grp", (96, 160)),
    (b"zerg\\zavdeath.grp", (36, 36)),
    (b"zerg\\zavexplo.grp", (48, 48)),
    (b"zerg\\brood.grp", (48, 48)),
    (b"zerg\\zbrshad.grp", (48, 48)),
    (b"zerg\\zbrdeath.grp", (48, 48)),
    (b"zerg\\bugguy.grp", (64, 64)),
    (b"zerg\\zbgshad.grp", (64, 64)),
    (b"thingy\\zbgexplo.grp", (80, 80)),
    (b"zerg\\cocoon.grp", (96, 96)),
    (b"zerg\\defiler.grp", (80, 80)),
    (b"zerg\\zdebirth.grp", (96, 160)),
    (b"zerg\\zdedeath.grp", (80, 80)),
    (b"zerg\\drone.grp", (128, 128)),
    (b"zerg\\zdrbirth.grp", (96, 160)),
    (b"zerg\\zdrdeath.grp", (128, 128)),
    (b"zerg\\egg.grp", (96, 96)),
    (b"zerg\\zegshad.grp", (96, 96)),
    (b"zerg\\zegspawn.grp", (96, 96)),
    (b"zerg\\zegdeath.grp", (96, 96)),
    (b"zerg\\guardian.grp", (96, 96)),
    (b"zerg\\zgudeath.grp", (96, 96)),
    (b"zerg\\hydra.grp", (128, 128)),
    (b"zerg\\zhyshad.grp", (128, 128)),
    (b"zerg\\zhybirth.grp", (96, 160)),
    (b"zerg\\zhydeath.grp", (128, 128)),
    (b"zerg\\uikerr.grp", (64, 64)),
    (b"zerg\\uikshad.grp", (64, 64)),
    (b"thingy\\spooge.grp", (128, 128)),
    (b"zerg\\larva.grp", (36, 36)),
    (b"zerg\\zladeath.grp", (64, 64)),
    (b"zerg\\mutalid.grp", (128, 128)),
    (b"zerg\\zmubirth.grp", (96, 160)),
    (b"zerg\\zmudeath.grp", (76, 76)),
    (b"zerg\\overlord.grp", (84, 84)),
    (b"zerg\\zovbirth.grp", (96, 160)),
    (b"zerg\\zovdeath.grp", (72, 72)),
    (b"zerg\\queen.grp", (128, 128)),
    (b"zerg\\zqudeath.grp", (96, 96)),
    (b"zerg\\zqubirth.grp", (96, 160)),
    (b"zerg\\ultra.grp", (128, 128)),
    (b"zerg\\zulshad.grp", (128, 128)),
    (b"zerg\\zulbirth.grp", (96, 160)),
    (b"zerg\\zuldeath.grp", (128, 128)),
    (b"zerg\\zergling.grp", (128, 128)),
    (b"zerg\\zzeshad.grp", (128, 128)),
    (b"zerg\\zzebirth.grp", (96, 160)),
    (b"zerg\\zzedeath.grp", (128, 128)),
    (b"thingy\\zairdthl.grp", (140, 140)),
    (b"thingy\\zairdths.grp", (80, 80)),
    (b"thingy\\zblddths.grp", (200, 200)),
    (b"zerg\\ucereb.grp", (160, 64)),
    (b"zerg\\zucshad.grp", (160, 64)),
    (b"terran\\control.grp", (128, 160)),
    (b"zerg\\chrysal.grp", (96, 128)),
    (b"zerg\\zchshad.grp", (96, 128)),
    (b"zerg\\cerebrat.grp", (96, 128)),
    (b"zerg\\zceshad.grp", (128, 160)),
    (b"zerg\\fcolony.grp", (128, 64)),
    (b"zerg\\zfcshad.grp", (128, 96)),
    (b"zerg\\hatchery.grp", (192, 160)),
    (b"zerg\\zhashad.grp", (192, 160)),
    (b"zerg\\hive.grp", (192, 224)),
    (b"zerg\\zhishad.grp", (192, 224)),
    (b"zerg\\lair.grp", (192, 160)),
    (b"zerg\\zlrshad.grp", (192, 160)),
    (b"zerg\\lurker.grp", (128, 128)),
    (b"zerg\\zlushad.grp", (128, 128)),
    (b"neutral\\kerrchry.grp", (64, 64)),
    (b"neutral\\nkoshad.grp", (64, 64)),
    (b"zerg\\mutacham.grp", (128, 192)),
    (b"zerg\\zmcshad.grp", (128, 192)),
    (b"zerg\\mutapit.grp", (128, 128)),
    (b"zerg\\zmhshad.grp", (128, 128)),
    (b"zerg\\nest.grp", (96, 128)),
    (b"zerg\\zneshad.grp", (96, 128)),
    (b"zerg\\nyduspit.grp", (128, 128)),
    (b"zerg\\znyshad.grp", (128, 128)),
    (b"zerg\\over1.grp", (224, 160)),
    (b"zerg\\over2.grp", (224, 160)),
    (b"zerg\\rcluster.grp", (160, 128)),
    (b"zerg\\zrcshad.grp", (160, 128)),
    (b"zerg\\extract.grp", (128, 192)),
    (b"zerg\\zreshad.grp", (128, 192)),
    (b"zerg\\snakey.grp", (160, 128)),
    (b"zerg\\zsbshad.grp", (160, 128)),
    (b"zerg\\spire.grp", (128, 128)),
    (b"zerg\\zspshad.grp", (128, 128)),
    (b"zerg\\scolony.grp", (128, 128)),
    (b"zerg\\zscshad.grp", (128, 128)),
    (b"zerg\\infest03.grp", (128, 128)),
    (b"zerg\\zbuild.grp", (160, 192)),
    (b"zerg\\zbshad.grp", (160, 192)),
    (b"zerg\\zspawn01.grp", (128, 96)),
    (b"zerg\\zspawn02.grp", (128, 128)),
    (b"zerg\\zspawn03.grp", (160, 192)),
    (b"thingy\\zrubbles.grp", (96, 96)),
    (b"thingy\\zrubblel.grp", (128, 128)),
    (b"protoss\\carrier.grp", (128, 128)),
    (b"thingy\\pcaglow.grp", (128, 128)),
    (b"protoss\\intercep.grp", (32, 32)),
    (b"protoss\\shuttle.grp", (60, 60)),
    (b"thingy\\pshglow.grp", (60, 60)),
    (b"protoss\\dragoon.grp", (96, 96)),
    (b"protoss\\pdrshad.grp", (96, 96)),
    (b"protoss\\pdrdeath.grp", (96, 96)),
    (b"protoss\\templar.grp", (128, 128)),
    (b"protoss\\pteshad.grp", (128, 128)),
    (b"protoss\\dtemplar.grp", (64, 64)),
    (b"protoss\\arbiter.grp", (76, 76)),
    (b"thingy\\pabglow.grp", (76, 76)),
    (b"protoss\\archon.grp", (120, 120)),
    (b"protoss\\archont.grp", (120, 120)),
    (b"protoss\\archont2.grp", (120, 120)),
    (b"protoss\\probe.grp", (32, 32)),
    (b"protoss\\scout.grp", (72, 72)),
    (b"thingy\\pscglow.grp", (72, 72)),
    (b"protoss\\trilob.grp", (84, 84)),
    (b"protoss\\ptrshad.grp", (84, 84)),
    (b"protoss\\sapper.grp", (20, 20)),
    (b"protoss\\witness.grp", (40, 40)),
    (b"protoss\\zealot.grp", (128, 128)),
    (b"protoss\\pzeshad.grp", (128, 128)),
    (b"protoss\\archives.grp", (160, 192)),
    (b"protoss\\pacshad.grp", (160, 192)),
    (b"protoss\\assim.grp", (192, 192)),
    (b"protoss\\passhad.grp", (192, 192)),
    (b"protoss\\beacon.grp", (96, 128)),
    (b"protoss\\pbeshad.grp", (160, 128)),
    (b"protoss\\citadel.grp", (160, 128)),
    (b"protoss\\pcishad.grp", (160, 128)),
    (b"protoss\\forge.grp", (160, 128)),
    (b"protoss\\forget.grp", (160, 128)),
    (b"protoss\\pfoshad.grp", (160, 128)),
    (b"protoss\\gateway.grp", (128, 160)),
    (b"protoss\\pgashad.grp", (128, 160)),
    (b"protoss\\gencore.grp", (96, 128)),
    (b"protoss\\gencoret.grp", (96, 128)),
    (b"protoss\\pgcshad.grp", (96, 128)),
    (b"neutral\\khyad01.grp", (128, 128)),
    (b"protoss\\nexus.grp", (192, 224)),
    (b"protoss\\pneglow.grp", (192, 224)),
    (b"protoss\\pneshad.grp", (192, 224)),
    (b"protoss\\photon.grp", (64, 128)),
    (b"protoss\\ppbshad.grp", (64, 128)),
    (b"protoss\\prism.grp", (96, 128)),
    (b"protoss\\paushad.grp", (96, 128)),
    (b"protoss\\pylon.grp", (64, 64)),
    (b"protoss\\ppyshad.grp", (64, 128)),
    (b"protoss\\robotic.grp", (96, 128)),
    (b"protoss\\proshad.grp", (128, 144)),
    (b"protoss\\sbattery.grp", (96, 64)),
    (b"protoss\\pbaglow.grp", (96, 64)),
    (b"protoss\\pbashad.grp", (96, 64)),
    (b"protoss\\stargate.grp", (128, 160)),
    (b"protoss\\psgglow.grp", (128, 160)),
    (b"protoss\\psgshad.grp", (128, 160)),
    (b"neutral\\stasis.grp", (128, 160)),
    (b"protoss\\stasis.grp", (160, 128)),
    (b"protoss\\pstshad.grp", (160, 128)),
    (b"neutral\\temple.grp", (224, 160)),
    (b"protoss\\warp.grp", (192, 160)),
    (b"protoss\\texture.grp", (192, 192)),
    (b"protoss\\pb1glow.grp", (152, 152)),
    (b"protoss\\pwashad.grp", (192, 160)),
    (b"thingy\\tbangs.grp", (128, 128)),
    (b"thingy\\tbangl.grp", (200, 200)),
    (b"thingy\\tbangx.grp", (252, 200)),
    (b"thingy\\prubbles.grp", (96, 96)),
    (b"thingy\\prubblel.grp", (128, 128)),
    (b"terran\\battlecr.grp", (120, 120)),
    (b"thingy\\tbaglow.grp", (120, 120)),
    (b"neutral\\civilian.grp", (64, 64)),
    (b"neutral\\ncishad.grp", (64, 64)),
    (b"terran\\dropship.grp", (60, 60)),
    (b"thingy\\tdrglow.grp", (68, 68)),
    (b"terran\\firebat.grp", (32, 32)),
    (b"terran\\tfbshad.grp", (32, 32)),
    (b"terran\\ghost.grp", (64, 64)),
    (b"terran\\tghshad.grp", (64, 64)),
    (b"terran\\tghdeath.grp", (64, 64)),
    (b"thingy\\nukebeam.grp", (44, 44)),
    (b"thingy\\nuketarg.grp", (8, 8)),
    (b"terran\\goliath.grp", (76, 76)),
    (b"terran\\goliatht.grp", (76, 76)),
    (b"terran\\tgoshad.grp", (76, 76)),
    (b"terran\\ughost.grp", (64, 64)),
    (b"terran\\ughshad.grp", (64, 64)),
    (b"terran\\marine.grp", (64, 64)),
    (b"terran\\tmashad.grp", (44, 44)),
    (b"terran\\tmadeath.grp", (64, 64)),
    (b"terran\\phoenix.grp", (64, 64)),
    (b"thingy\\tphglow.grp", (64, 64)),
    (b"terran\\scv.grp", (72, 72)),
    (b"thingy\\tscglow.grp", (72, 72)),
    (b"terran\\tank.grp", (128, 128)),
    (b"terran\\tankt.grp", (128, 128)),
    (b"terran\\ttashad.grp", (128, 128)),
    (b"terran\\stank.grp", (128, 128)),
    (b"terran\\stankt.grp", (128, 128)),
    (b"terran\\tstshad.grp", (128, 128)),
    (b"terran\\vulture.grp", (100, 100)),
    (b"terran\\spider.grp", (36, 36)),
    (b"terran\\tsmshad.grp", (36, 36)),
    (b"terran\\wessel.grp", (96, 128)),
    (b"terran\\wesselt.grp", (100, 100)),
    (b"terran\\tveshad.grp", (96, 128)),
    (b"terran\\academy.grp", (96, 128)),
    (b"terran\\academyt.grp", (96, 128)),
    (b"terran\\tacshad.grp", (96, 128)),
    (b"terran\\tbarrack.grp", (192, 160)),
    (b"terran\\tbrshad.grp", (192, 160)),
    (b"terran\\chemlab.grp", (160, 128)),
    (b"terran\\chemlabt.grp", (160, 128)),
    (b"terran\\tclshad.grp", (160, 128)),
    (b"terran\\comsat.grp", (128, 64)),
    (b"terran\\comsatc.grp", (128, 64)),
    (b"terran\\comsatt.grp", (128, 64)),
    (b"terran\\tcsshad.grp", (128, 64)),
    (b"terran\\controlt.grp", (128, 160)),
    (b"terran\\tccshad.grp", (128, 160)),
    (b"terran\\depot.grp", (96, 128)),
    (b"terran\\depott.grp", (96, 128)),
    (b"terran\\tdeshad.grp", (96, 128)),
    (b"terran\\drydocks.grp", (128, 64)),
    (b"terran\\drydockc.grp", (128, 64)),
    (b"terran\\drydockt.grp", (128, 64)),
    (b"terran\\tddshad.grp", (128, 64)),
    (b"terran\\factory.grp", (128, 160)),
    (b"terran\\factoryt.grp", (128, 160)),
    (b"terran\\tfashad.grp", (128, 160)),
    (b"terran\\genelab.grp", (128, 128)),
    (b"terran\\genelabc.grp", (128, 128)),
    (b"terran\\genelabt.grp", (128, 128)),
    (b"terran\\tglshad.grp", (128, 128)),
    (b"neutral\\ion.grp", (256, 160)),
    (b"terran\\machines.grp", (128, 128)),
    (b"terran\\machinec.grp", (128, 128)),
    (b"terran\\tmsshad.grp", (128, 128)),
    (b"terran\\missile.grp", (128, 128)),
    (b"terran\\missilet.grp", (128, 128)),
    (b"terran\\tmishad.grp", (128, 128)),
    (b"neutral\\cbattle.grp", (160, 128)),
    (b"neutral\\cbashad.grp", (160, 128)),
    (b"terran\\physics.grp", (128, 64)),
    (b"terran\\physicsc.grp", (128, 64)),
    (b"terran\\tplshad.grp", (128, 128)),
    (b"terran\\pillbox.grp", (96, 128)),
    (b"terran\\tpbshad.grp", (96, 128)),
    (b"terran\\pillboxt.grp", (64, 64)),
    (b"terran\\refinery.grp", (192, 192)),
    (b"terran\\treshad.grp", (192, 192)),
    (b"terran\\research.grp", (128, 96)),
    (b"terran\\researct.grp", (128, 96)),
    (b"terran\\trlshad.grp", (128, 96)),
    (b"terran\\nukesilo.grp", (128, 64)),
    (b"terran\\nukesilc.grp", (128, 64)),
    (b"terran\\nukesilt.grp", (128, 64)),
    (b"terran\\tnsshad.grp", (128, 64)),
    (b"terran\\nukemiss.grp", (48, 40)),
    (b"thingy\\nukehit.grp", (252, 252)),
    (b"terran\\starport.grp", (128, 160)),
    (b"terran\\starpot.grp", (128, 160)),
    (b"terran\\tspshad.grp", (128, 160)),
    (b"terran\\weaponpl.grp", (192, 160)),
    (b"terran\\weaponpt.grp", (192, 160)),
    (b"terran\\twpshad.grp", (192, 160)),
    (b"terran\\tbldlrg.grp", (160, 128)),
    (b"terran\\tb2shad.grp", (128, 128)),
    (b"terran\\tbldmed.grp", (96, 96)),
    (b"terran\\tb3shad.grp", (96, 96)),
    (b"terran\\tbldsml.grp", (96, 96)),
    (b"terran\\tb1shad.grp", (96, 96)),
    (b"thingy\\rubbles.grp", (96, 96)),
    (b"thingy\\rubblel.grp", (128, 128)),
    (b"thingy\\dswarm.grp", (252, 188)),
    (b"neutral\\acritter.grp", (104, 104)),
    (b"neutral\\nacshad.grp", (104, 104)),
    (b"neutral\\bcritter.grp", (104, 104)),
    (b"neutral\\nbcshad.grp", (104, 104)),
    (b"neutral\\jcritter.grp", (128, 128)),
    (b"neutral\\njcshad.grp", (128, 128)),
    (b"neutral\\geyser.grp", (128, 64)),
    (b"neutral\\geyshad.grp", (160, 64)),
    (b"neutral\\min01.grp", (64, 96)),
    (b"neutral\\min01sha.grp", (128, 96)),
    (b"neutral\\min02.grp", (64, 96)),
    (b"neutral\\min02sha.grp", (128, 96)),
    (b"neutral\\min03.grp", (64, 96)),
    (b"neutral\\min03sha.grp", (128, 96)),
    (b"zerg\\zmarker.grp", (100, 80)),
    (b"zerg\\zcirglow.grp", (100, 80)),
    (b"terran\\tmarker.grp", (128, 128)),
    (b"terran\\tcirglow.grp", (128, 128)),
    (b"protoss\\pmarker.grp", (100, 80)),
    (b"protoss\\pcirglow.grp", (100, 80)),
    (b"thingy\\eldfire.grp", (64, 64)),
    (b"thingy\\eldsmall.grp", (72, 72)),
    (b"thingy\\eldmed.grp", (84, 84)),
    (b"thingy\\eldlarge.grp", (108, 108)),
    (b"thingy\\esfhit.grp", (128, 128)),
    (b"thingy\\esfsmall.grp", (80, 80)),
    (b"thingy\\esflarge.grp", (128, 128)),
    (b"thingy\\sbasmall.grp", (64, 64)),
    (b"thingy\\sbalarge.grp", (64, 64)),
    (b"thingy\\edmfrons.grp", (72, 72)),
    (b"thingy\\edmfronm.grp", (84, 84)),
    (b"thingy\\edmfronl.grp", (108, 108)),
    (b"thingy\\edmbacks.grp", (72, 72)),
    (b"thingy\\edmbackm.grp", (84, 84)),
    (b"thingy\\edmbackl.grp", (108, 108)),
    (b"thingy\\edmhits.grp", (72, 72)),
    (b"thingy\\edmhitm.grp", (84, 84)),
    (b"thingy\\edmhitl.grp", (108, 108)),
    (b"thingy\\eradlrg.grp", (120, 120)),
    (b"thingy\\ensnare.grp", (128, 128)),
    (b"thingy\\ensgoos.grp", (24, 24)),
    (b"thingy\\ensgoom.grp", (32, 32)),
    (b"thingy\\ensgool.grp", (56, 56)),
    (b"thingy\\ebbcloud.grp", (128, 128)),
    (b"thingy\\ebbhits.grp", (24, 24)),
    (b"thingy\\ebbhitm.grp", (32, 32)),
    (b"thingy\\ebbhitl.grp", (56, 56)),
    (b"thingy\\recall.grp", (100, 100)),
    (b"neutral\\flag.grp", (32, 32)),
    (b"neutral\\kerregg.grp", (64, 64)),
    (b"neutral\\psiemit.grp", (24, 24)),
    (b"neutral\\datadisk.grp", (20, 20)),
    (b"neutral\\khchunk.grp", (28, 28)),
    (b"neutral\\orechunk.grp", (32, 32)),
    (b"neutral\\gasorb.grp", (32, 32)),
    (b"neutral\\gassac.grp", (32, 32)),
    (b"neutral\\gastank.grp", (28, 28)),
    (b"neutral\\norshad.grp", (32, 32)),
    (b"neutral\\ngoshad.grp", (32, 32)),
    (b"neutral\\ngsshad.grp", (32, 32)),
    (b"neutral\\ngcshad.grp", (28, 28)),
    (b"neutral\\nddshad.grp", (32, 32)),
    (b"neutral\\nddsha2.grp", (32, 32)),
    (b"neutral\\nflshad.grp", (32, 32)),
    (b"neutral\\nflsha2.grp", (32, 32)),
    (b"neutral\\nkhshad.grp", (36, 36)),
    (b"neutral\\nkhsha2.grp", (36, 36)),
    (b"neutral\\nkeshad.grp", (64, 64)),
    (b"neutral\\nkesha2.grp", (64, 64)),
    (b"neutral\\npsshad.grp", (40, 40)),
    (b"neutral\\npssha2.grp", (40, 40)),
    (b"thingy\\plasma.grp", (256, 256)),
    (b"thingy\\plasdrip.grp", (60, 60)),
    (b"thingy\\flamer.grp", (224, 224)),
    (b"thingy\\smoke.grp", (84, 84)),
    (b"thingy\\bdust.grp", (128, 128)),
    (b"thingy\\pshield.grp", (64, 64)),
    (b"thingy\\explo2.grp", (24, 28)),
    (b"thingy\\dbl_exp.grp", (32, 32)),
    (b"thingy\\hkexplod.grp", (40, 40)),
    (b"thingy\\small.grp", (44, 60)),
    (b"thingy\\tmnexplo.grp", (80, 80)),
    (b"thingy\\geysmok1.grp", (32, 64)),
    (b"thingy\\geysmok2.grp", (32, 64)),
    (b"thingy\\geysmok3.grp", (32, 64)),
    (b"thingy\\geysmok4.grp", (32, 64)),
    (b"thingy\\geysmok5.grp", (32, 64)),
    (b"thingy\\geysmos1.grp", (32, 64)),
    (b"thingy\\efghit.grp", (56, 56)),
    (b"thingy\\gresmoke.grp", (64, 64)),
    (b"thingy\\hktrail.grp", (84, 84)),
    (b"thingy\\psaexplo.grp", (80, 80)),
    (b"thingy\\blackx.grp", (44, 28)),
    (b"thingy\\elbbat.grp", (120, 120)),
    (b"thingy\\elbhit.grp", (40, 40)),
    (b"thingy\\elbmuzz.grp", (112, 112)),
    (b"thingy\\pteglow.grp", (128, 128)),
    (b"thingy\\ofirec.grp", (64, 96)),
    (b"thingy\\ofiref.grp", (64, 96)),
    (b"thingy\\ofirev.grp", (64, 96)),
    (b"thingy\\bblood01.grp", (64, 96)),
    (b"thingy\\bblood02.grp", (64, 96)),
    (b"thingy\\bblood03.grp", (64, 96)),
    (b"thingy\\bblood04.grp", (64, 96)),
    (b"thingy\\dust01.grp", (48, 48)),
    (b"thingy\\dust02.grp", (48, 48)),
    (b"thingy\\dust03.grp", (48, 48)),
    (b"thingy\\dust04.grp", (48, 48)),
    (b"thingy\\dust05.grp", (48, 48)),
    (b"thingy\\dust06.grp", (48, 48)),
    (b"thingy\\dust07.grp", (48, 48)),
    (b"thingy\\dust08.grp", (48, 48)),
    (b"thingy\\dust09.grp", (48, 48)),
    (b"bullet\\circle14.grp", (32, 32)),
    (b"bullet\\zspark.grp", (40, 40)),
    (b"bullet\\pdriphit.grp", (52, 52)),
    (b"bullet\\tentacle.grp", (128, 128)),
    (b"thingy\\ep2fire.grp", (80, 80)),
    (b"thingy\\etghit.grp", (44, 44)),
    (b"thingy\\ep3shot.grp", (20, 20)),
    (b"thingy\\ep3burst.grp", (100, 100)),
    (b"bullet\\spores.grp", (36, 36)),
    (b"thingy\\sporehit.grp", (72, 72)),
    (b"thingy\\spotrail.grp", (28, 28)),
    (b"thingy\\gsmoke.grp", (84, 84)),
    (b"bullet\\spore2.grp", (24, 24)),
    (b"bullet\\parasite.grp", (20, 20)),
    (b"thingy\\consume.grp", (72, 72)),
    (b"thingy\\eplmuzz.grp", (128, 128)),
    (b"bullet\\pspark.grp", (40, 40)),
    (b"bullet\\ephfire.grp", (48, 32)),
    (b"bullet\\hks.grp", (20, 20)),
    (b"bullet\\blastcan.grp", (32, 32)),
    (b"bullet\\dragbull.grp", (32, 32)),
    (b"bullet\\epbbul.grp", (64, 64)),
    (b"thingy\\psistorm.grp", (224, 224)),
    (b"bullet\\scvspark.grp", (48, 48)),
    (b"bullet\\tspark.grp", (40, 40)),
    (b"bullet\\gemini.grp", (36, 36)),
    (b"bullet\\missile.grp", (32, 32)),
    (b"thingy\\ecahit.grp", (48, 48)),
    (b"bullet\\grenade.grp", (16, 16)),
    (b"thingy\\mushroom.grp", (100, 108)),
    (b"thingy\\elbfire.grp", (128, 128)),
    (b"thingy\\elbfirew.grp", (64, 64)),
    (b"thingy\\ettflash.grp", (128, 128)),
    (b"thingy\\esifire.grp", (72, 72)),
    (b"thingy\\tveglowb.grp", (96, 128)),
    (b"thingy\\tveglowo.grp", (96, 128)),
    (b"thingy\\tvegloww.grp", (96, 128)),
    (b"bullet\\eycbull.grp", (96, 96)),
    (b"thingy\\eycblast.grp", (160, 160)),
    (b"thingy\\eychit.grp", (144, 144)),
    (b"thingy\\halmind.grp", (64, 64)),
    (b"thingy\\evecast.grp", (48, 48)),
    (b"thingy\\emshit.grp", (80, 80)),
    (b"thingy\\emsbeam.grp", (192, 192)),
    (b"thingy\\elect02a.grp", (96, 96)),
    (b"thingy\\elect02.grp", (96, 96)),
    (b"thingy\\emp.grp", (252, 252)),
    (b"thingy\\empl.grp", (252, 252)),
    (b"thingy\\ehamed.grp", (96, 96)),
    (b"thingy\\o022.grp", (32, 32)),
    (b"thingy\\o032.grp", (40, 40)),
    (b"thingy\\o048.grp", (64, 64)),
    (b"thingy\\o062.grp", (64, 64)),
    (b"thingy\\o072.grp", (80, 80)),
    (b"thingy\\o094.grp", (96, 96)),
    (b"thingy\\o110.grp", (128, 128)),
    (b"thingy\\o122.grp", (128, 128)),
    (b"thingy\\o146.grp", (148, 148)),
    (b"thingy\\o224.grp", (224, 224)),
    (b"thingy\\od022.grp", (36, 36)),
    (b"thingy\\od032.grp", (48, 48)),
    (b"thingy\\od048.grp", (72, 72)),
    (b"thingy\\od062.grp", (72, 72)),
    (b"thingy\\od072.grp", (92, 92)),
    (b"thingy\\od094.grp", (108, 108)),
    (b"thingy\\od110.grp", (144, 144)),
    (b"thingy\\od122.grp", (144, 144)),
    (b"thingy\\od146.grp", (168, 168)),
    (b"thingy\\od224.grp", (252, 252)),
    (b"neutral\\maprev.grp", (32, 32)),
    (b"thingy\\juice01.grp", (252, 154)),
    (b"thingy\\juice02.grp", (252, 154)),
    (b"thingy\\startloc.grp", (128, 96)),
    (b"thingy\\tileset\\ashworld\\rock01.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock1sha.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock02.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock2sha.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock03.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock3sha.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock04.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock4sha.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock05.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\rock5sha.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\lalroc01.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\lalroc02.grp", (128, 192)),
    (b"thingy\\tileset\\ashworld\\hasroc01.grp", (128, 160)),
    (b"thingy\\tileset\\ashworld\\hasroc02.grp", (128, 128)),
    (b"thingy\\tileset\\ashworld\\hasroc03.grp", (128, 128)),
    (b"thingy\\tileset\\ashworld\\hasroc04.grp", (128, 128)),
    (b"thingy\\tileset\\ashworld\\hasroc05.grp", (128, 160)),
    (b"thingy\\tileset\\ashworld\\hasroc06.grp", (128, 160)),
    (b"thingy\\tileset\\ashworld\\rorock01.grp", (128, 128)),
    (b"thingy\\tileset\\ashworld\\rorock02.grp", (128, 128)),
    (b"thingy\\tileset\\ashworld\\rorock03.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\hdrock01.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock1s.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock02.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock2s.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock03.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock3s.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock04.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\hdrock4s.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\jubush01.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\jubush1s.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\jubush03.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\jubush3s.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\jubush05.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\jubush5s.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\ldtree01.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree1s.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree02.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree2s.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree03.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree3s.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree04.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\ldtree4s.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\tree01.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree1sha.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree02.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree2sha.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree03.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree3sha.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree04.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\tree4sha.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\dd025.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd026.grp", (64, 64)),
    (b"thingy\\tileset\\jungle\\dd027.grp", (128, 104)),
    (b"thingy\\tileset\\jungle\\dd028.grp", (128, 104)),
    (b"thingy\\tileset\\jungle\\dd029.grp", (128, 104)),
    (b"thingy\\tileset\\jungle\\dd030.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd031.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd055.grp", (128, 128)),
    (b"thingy\\tileset\\jungle\\dd056.grp", (64, 96)),
    (b"thingy\\tileset\\jungle\\dd075.grp", (128, 104)),
    (b"thingy\\tileset\\jungle\\dd076.grp", (128, 104)),
    (b"thingy\\tileset\\jungle\\dd077.grp", (128, 104)),
    (b"thingy\\tileset\\jungle\\dd078.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd079.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd080.grp", (128, 64)),
    (b"thingy\\tileset\\jungle\\dd081.grp", (128, 64)),
    (b"thingy\\tileset\\jungle\\dd091.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd203.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd204.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd205.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd206.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd207.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd209.grp", (192, 192)),
    (b"thingy\\tileset\\jungle\\dd210.grp", (128, 96)),
    (b"thingy\\tileset\\jungle\\dd211.grp", (128, 96)),
    (b"thingy\\tileset\\platform\\dish01.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\dish1sha.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\dish02.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\dish2sha.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\dish03.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\dish3sha.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\glob01.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\glob1sha.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\glob02.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\glob2sha.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\glob03.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\glob3sha.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\towr01.grp", (64, 224)),
    (b"thingy\\tileset\\platform\\towr02.grp", (64, 224)),
    (b"thingy\\tileset\\platform\\tree01.grp", (128, 192)),
    (b"thingy\\tileset\\platform\\tree02.grp", (128, 224)),
    (b"thingy\\tileset\\platform\\lbsign01.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign02.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign03.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign04.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign05.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign06.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign07.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\lbsign08.grp", (192, 224)),
    (b"thingy\\tileset\\platform\\spthin01.grp", (128, 96)),
    (b"thingy\\tileset\\platform\\refinery.grp", (128, 128)),
    (b"thingy\\tileset\\badlands\\hdrock01.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock1s.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock02.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock2s.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock03.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock3s.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock04.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock4s.grp", (64, 96)),
    (b"thingy\\tileset\\badlands\\hdrock05.grp", (128, 128)),
    (b"thingy\\tileset\\badlands\\hdrock06.grp", (128, 128)),
    (b"thingy\\tileset\\badlands\\hdrock07.grp", (128, 128)),
    (b"thingy\\tileset\\badlands\\hdrock08.grp", (128, 128)),
    (b"thingy\\tileset\\badlands\\hdvent01.grp", (128, 96)),
    (b"thingy\\tileset\\badlands\\hdtree01.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\ldtree1s.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\hdtree02.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\ldtree2s.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\hdtree03.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\ldtree3s.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\hdtree04.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\ldtree4s.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\hgtree01.grp", (192, 192)),
    (b"thingy\\tileset\\badlands\\lcshop01.grp", (192, 160)),
    (b"thingy\\tileset\\badlands\\lcshop02.grp", (192, 160)),
    (b"thingy\\tileset\\badlands\\lcshop03.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcshop04.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcshop05.grp", (192, 160)),
    (b"thingy\\tileset\\badlands\\lcshop06.grp", (192, 160)),
    (b"thingy\\tileset\\badlands\\lcshop07.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcshop08.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcshop09.grp", (192, 160)),
    (b"thingy\\tileset\\badlands\\lcshopaa.grp", (192, 160)),
    (b"thingy\\tileset\\badlands\\lcsign01.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign02.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign03.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign04.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign05.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign06.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign07.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign08.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsign09.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsignaa.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsignbb.grp", (192, 224)),
    (b"thingy\\tileset\\badlands\\lcsigncc.grp", (192, 224)),
    (b"thingy\\tileset\\install\\clplate2.grp", (64, 96)),
    (b"thingy\\tileset\\install\\clplate1.grp", (64, 96)),
    (b"thingy\\tileset\\install\\clplat1t.grp", (64, 96)),
    (b"thingy\\tileset\\install\\dcgun1.grp", (64, 96)),
    (b"thingy\\tileset\\install\\dcgun2.grp", (64, 96)),
    (b"thingy\\tileset\\install\\crdoor1.grp", (192, 160)),
    (b"thingy\\tileset\\install\\dcfan1.grp", (64, 96)),
    (b"thingy\\tileset\\install\\dicran1.grp", (128, 128)),
    (b"thingy\\tileset\\install\\dicran2.grp", (128, 128)),
    (b"thingy\\tileset\\install\\dicran3.grp", (128, 128)),
    (b"thingy\\tileset\\install\\dicran4.grp", (128, 128)),
    (b"thingy\\tileset\\install\\didoor1.grp", (192, 160)),
    (b"thingy\\tileset\\install\\digear1.grp", (64, 64)),
    (b"thingy\\tileset\\install\\digear2.grp", (64, 64)),
    (b"thingy\\tileset\\install\\dihatc1.grp", (256, 128)),
    (b"thingy\\tileset\\twilight\\ldarch.grp", (128, 96)),
    (b"thingy\\tileset\\twilight\\lddrill.grp", (128, 96)),
    (b"thingy\\tileset\\twilight\\ldxel01.grp", (128, 96)),
    (b"thingy\\tileset\\twilight\\ldxel02.grp", (128, 128)),
    (b"thingy\\tileset\\twilight\\ldxel03.grp", (128, 192)),
    (b"thingy\\tileset\\twilight\\ldxel04.grp", (128, 192)),
    (b"thingy\\tileset\\twilight\\ldxel05.grp", (64, 192)),
    (b"thingy\\tileset\\twilight\\ldxel06.grp", (128, 160)),
    (b"thingy\\tileset\\twilight\\ldxeltur.grp", (128, 128)),
    (b"thingy\\tileset\\twilight\\jtree01.grp", (64, 160)),
    (b"thingy\\tileset\\twilight\\jtree02.grp", (64, 160)),
    (b"thingy\\tileset\\twilight\\jtree03.grp", (64, 96)),
    (b"thingy\\tileset\\twilight\\jtree04.grp", (64, 192)),
    (b"thingy\\tileset\\twilight\\jtree05.grp", (64, 160)),
    (b"thingy\\tileset\\twilight\\rstatue.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\hdbld01.grp", (128, 192)),
    (b"thingy\\tileset\\ice\\shdbld01.grp", (128, 192)),
    (b"thingy\\tileset\\ice\\hdbld02.grp", (128, 192)),
    (b"thingy\\tileset\\ice\\shdbld02.grp", (128, 192)),
    (b"thingy\\tileset\\ice\\hdbld03.grp", (128, 192)),
    (b"thingy\\tileset\\ice\\shdbld03.grp", (128, 192)),
    (b"thingy\\tileset\\ice\\hdbld04.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\shdbld04.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\hdradarl.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\shdradrl.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\hdradarr.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\shdradrr.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\hdrock01.grp", (128, 96)),
    (b"thingy\\tileset\\ice\\shdrck01.grp", (128, 96)),
    (b"thingy\\tileset\\ice\\hdrock02.grp", (128, 96)),
    (b"thingy\\tileset\\ice\\shdrck02.grp", (128, 96)),
    (b"thingy\\tileset\\ice\\hdstre01.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\hdstre02.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\hdstre03.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\hdstre04.grp", (64, 128)),
    (b"thingy\\tileset\\ice\\hdspire.grp", (64, 160)),
    (b"thingy\\tileset\\ice\\shdspire.grp", (64, 160)),
    (b"thingy\\tileset\\ice\\hdtwr01.grp", (64, 160)),
    (b"thingy\\tileset\\ice\\shdtwr01.grp", (64, 160)),
    (b"thingy\\tileset\\ice\\hdtwr02.grp", (64, 160)),
    (b"thingy\\tileset\\ice\\shdtwr02.grp", (64, 160)),
    (b"thingy\\tileset\\ice\\ldbtre01.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\sldbtre1.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\ldbtre02.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\sldbtre2.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\ldbtre03.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\sldbtre3.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\ldbtre04.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\sldbtre4.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\ldbld01.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\sldbld01.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\ldbld02.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\sldbld02.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\ldcomm.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\sldcomm.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\lddtre01.grp", (64, 256)),
    (b"thingy\\tileset\\ice\\slddtre1.grp", (64, 256)),
    (b"thingy\\tileset\\ice\\lddtre02.grp", (64, 224)),
    (b"thingy\\tileset\\ice\\slddtre2.grp", (64, 224)),
    (b"thingy\\tileset\\ice\\lddish.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\slddish.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\ldrdr01.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\sldrdr01.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\ldrdr02.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\sldrdr02.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\ldrdr03.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\sldrdr03.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\ldrck01.grp", (128, 96)),
    (b"thingy\\tileset\\ice\\sldrck01.grp", (128, 96)),
    (b"thingy\\tileset\\ice\\ldrck02.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\sldrck02.grp", (128, 128)),
    (b"thingy\\tileset\\ice\\hdpipes.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\shdpipes.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\ldthing.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\sldthing.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\ldsmrock.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\hdradr02.grp", (64, 128)),
    (b"thingy\\tileset\\ice\\shdradr2.grp", (64, 128)),
    (b"thingy\\tileset\\ice\\jgant1.grp", (64, 96)),
    (b"thingy\\tileset\\ice\\rjbtree1.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\srjbtre1.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\rjbtree2.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\srjbtre2.grp", (128, 224)),
    (b"thingy\\tileset\\ice\\rjbtree3.grp", (128, 160)),
    (b"thingy\\tileset\\ice\\srjbtre3.grp", (128, 160)),
    (b"thingy\\tileset\\ice\\rjbtree4.grp", (128, 160)),
    (b"thingy\\tileset\\ice\\srjbtre4.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\hdbant.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\hdbbroke.grp", (128, 96)),
    (b"thingy\\tileset\\desert\\shdbbrke.grp", (128, 96)),
    (b"thingy\\tileset\\desert\\hdbgas.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\shdbgas.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\hdbmed.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\shdbmed.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\hdbmoss.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\shdbmoss.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\hdbtent.grp", (128, 192)),
    (b"thingy\\tileset\\desert\\shdbtent.grp", (128, 192)),
    (b"thingy\\tileset\\desert\\hdplnt03.grp", (96, 64)),
    (b"thingy\\tileset\\desert\\shdplnt3.grp", (96, 64)),
    (b"thingy\\tileset\\desert\\jgbroke.grp", (128, 96)),
    (b"thingy\\tileset\\desert\\sjgbroke.grp", (128, 96)),
    (b"thingy\\tileset\\desert\\jgbgas.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sjgbgas.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\jgbgen.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sjgbgen.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\jgbsgn.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sjgbsgn.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\jgbtent.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\sjgbtent.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\jgbcomm.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\sjgbcomm.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\jgbfact.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sjgbfact.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\jgbred.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sjgbred.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\jgplnt01.grp", (64, 64)),
    (b"thingy\\tileset\\desert\\jgplnt02.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\sjgplnt2.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\ldbaz.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sldbaz.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\ldbgas.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\sldbgas.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\ldbgren.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sldbgren.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\ldbneon.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\ldbsuky.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\sldbsuky.grp", (128, 160)),
    (b"thingy\\tileset\\desert\\ldbtent.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\sldbtent.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\ldneon.grp", (128, 128)),
    (b"thingy\\tileset\\desert\\hdlbox01.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\shdlbox1.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\hdmachn2.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\ldlbox01.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\sldlbox1.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\ldmachn1.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\sldmchn1.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\ldplnt04.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\sldplnt4.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\tgas.grp", (64, 96)),
    (b"thingy\\tileset\\desert\\sarlacc.grp", (128, 64)),
    (b"zerg\\lurkegg.grp", (128, 128)),
    (b"zerg\\devour.grp", (96, 96)),
    (b"thingy\\zdvbirth.grp", (96, 96)),
    (b"zerg\\zdvdeath.grp", (96, 96)),
    (b"zerg\\zlubirth.grp", (128, 128)),
    (b"zerg\\zlurker.grp", (128, 128)),
    (b"zerg\\zlurker.grp", (128, 128)),
    (b"zerg\\zzlushad.grp", (128, 128)),
    (b"zerg\\xovermnd.grp", (224, 160)),
    (b"zerg\\zovshad.grp", (224, 160)),
    (b"protoss\\darchn.grp", (120, 120)),
    (b"protoss\\darchnt.grp", (120, 120)),
    (b"protoss\\darchnt2.grp", (120, 120)),
    (b"protoss\\pdadeath.grp", (160, 160)),
    (b"protoss\\corsair.grp", (60, 60)),
    (b"thingy\\pcsglow.grp", (60, 60)),
    (b"thingy\\elbfirec.grp", (60, 60)),
    (b"protoss\\xwarpgat.grp", (160, 240)),
    (b"protoss\\pwgshad.grp", (160, 240)),
    (b"protoss\\xwarpfir.grp", (160, 240)),
    (b"protoss\\xeltempl.grp", (256, 256)),
    (b"protoss\\pxtshad.grp", (256, 256)),
    (b"terran\\bomber.grp", (128, 128)),
    (b"thingy\\tbmglow.grp", (128, 128)),
    (b"thingy\\tbmafter.grp", (128, 128)),
    (b"terran\\medic.grp", (64, 64)),
    (b"terran\\tmeshad.grp", (64, 64)),
    (b"terran\\tmedeath.grp", (64, 64)),
    (b"neutral\\psidisr.grp", (224, 212)),
    (b"neutral\\tpdshad.grp", (224, 212)),
    (b"neutral\\generate.grp", (144, 144)),
    (b"neutral\\tgnshad.grp", (144, 144)),
    (b"thingy\\disrupt.grp", (160, 160)),
    (b"neutral\\kcritter.grp", (92, 92)),
    (b"neutral\\nckshad.grp", (92, 92)),
    (b"neutral\\scritter.grp", (92, 92)),
    (b"neutral\\icritter.grp", (92, 92)),
    (b"neutral\\ncicshad.grp", (92, 92)),
    (b"neutral\\uraj.grp", (28, 28)),
    (b"neutral\\khalis.grp", (28, 28)),
    (b"thingy\\bsmoke.grp", (84, 84)),
    (b"bullet\\spike.grp", (128, 128)),
    (b"thingy\\zdvpuke.grp", (80, 80)),
    (b"thingy\\zdvhit.grp", (64, 64)),
    (b"thingy\\pcssplsh.grp", (64, 64)),
    (b"bullet\\smmissle.grp", (32, 32)),
    (b"thingy\\tcuresml.grp", (64, 64)),
    (b"thingy\\tcuremed.grp", (64, 64)),
    (b"thingy\\tcurelrg.grp", (128, 128)),
    (b"thingy\\tmehealm.grp", (64, 64)),
    (b"thingy\\tmeheall.grp", (128, 128)),
    (b"thingy\\pdamyosm.grp", (32, 32)),
    (b"thingy\\pdamyomd.grp", (64, 64)),
    (b"thingy\\pdamyolg.grp", (128, 128)),
    (b"thingy\\tmeflshs.grp", (32, 32)),
    (b"thingy\\tmeflshm.grp", (64, 64)),
    (b"thingy\\tmeflshl.grp", (128, 128)),
    (b"thingy\\pdapsysm.grp", (64, 64)),
    (b"thingy\\pdapsymd.grp", (64, 64)),
    (b"thingy\\pdapsylg.grp", (128, 128)),
    (b"thingy\\pdaparsm.grp", (32, 32)),
    (b"thingy\\pdaparmd.grp", (64, 64)),
    (b"thingy\\pdaparlg.grp", (128, 128)),
    (b"thingy\\zdvgoos.grp", (24, 24)),
    (b"thingy\\zdvgoom.grp", (32, 32)),
    (b"thingy\\zdvgool.grp", (56, 56)),
    (b"thingy\\maelhit.grp", (128, 128)),
];

static LOOKUP: Lazy<FxHashMap<Vec<u8>, (u16, u16)>> = Lazy::new(|| {
    DEFAULT_GRP_SIZES
        .iter()
        .map(|x| (x.0.into(), x.1))
        .collect()
});

/// Path is expected to be in shape "unit\\terran\\scv.grp" or such
pub fn grp_default_size(path: &str) -> Option<(u16, u16)> {
    let key = path.replace("/", "\\");
    let mut key: String = key.trim_start_matches("unit\\").into();
    key.make_ascii_lowercase();
    LOOKUP.get(&key.into_bytes()).copied()
}