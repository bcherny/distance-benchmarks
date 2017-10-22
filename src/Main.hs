module Main where

import Data.List (zip4)
import Haversine (haversineDistance)
import Google (googleDistance)
import Mapbox (mapboxDistance)
import Taxicab (taxicabDistance)
import Units (Lat, Lng, Mi)

main :: IO ()
main = do

  let ps = take 50 $ drop 450 pairs

  haversines <- mapM (dist haversineDistance) ps
  taxicabs <- mapM (dist taxicabDistance) ps
  googles <- mapM (dist googleDistance) ps
  mapboxes <- mapM (dist mapboxDistance) ps

  putStrLn $ show $ zip4 haversines taxicabs googles mapboxes

dist :: ((Lat, Lng) -> (Lat, Lng) -> IO (Maybe Mi))
  -> ((Lat, Lng), (Lat, Lng))
  -> IO (Maybe Mi)
dist f (a, b) = f a b

pairs :: [((Lat, Lng), (Lat, Lng))]
pairs = zip coords $ reverse coords

coords :: [(Lat, Lng)]
coords = [(37.7847382, -122.4091996), (37.7347732, -122.4556654), (37.7879155, -122.4174338), (37.794013327475, -122.42979224608), (37.8010963, -122.4195558), (37.7590733, -122.4290386), (37.7925153, -122.4382307), (37.777463, -122.4131762), (37.7872457, -122.4187461), (37.7861649, -122.414424), (37.7536008, -122.4067403), (37.7925153, -122.4382307), (37.7592288, -122.4142591), (37.8301999, -122.2707319), (37.764764806022, -122.42396945363), (37.7907688, -122.4095738), (37.7814666, -122.4015577), (37.7828786, -122.4146017), (37.783352, -122.4739295), (37.7863965, -122.414493), (37.7852612, -122.414262), (37.77597, -122.41074599999999), (37.7852612, -122.414262), (37.727782000000005, -122.393851), (37.790507500000004, -122.3962536), (37.7831066, -122.4136744), (37.7892932, -122.4123523), (37.7965414, -122.4111801), (37.7847382, -122.4091996), (37.7925153, -122.4382307), (37.804643, -122.4428779), (37.7598648, -122.4147977), (37.7349074, -122.4168489), (37.785637900000005, -122.469242), (37.78718629999999, -122.4192291), (37.7892932, -122.4123523), (37.7976485, -122.4067342), (37.7858422, -122.4185638), (37.95657449999999, -122.5561917), (37.7824802, -122.4535476), (37.7852205, -122.4159761), (37.774773, -122.427361), (37.7839296, -122.4163165), (37.7892932, -122.4123523), (37.78718629999999, -122.4192291), (37.7925153, -122.4382307), (37.7925153, -122.4382307), (43.8941249,	-85.78739), (37.7508798, -122.4295106), (37.7357201, -122.4228093), (37.775908785269, -122.42452517712), (37.7852612, -122.414262), (37.78561699999999, -122.412155), (37.7847382, -122.4091996), (37.7480127, -122.4434925), (37.7981684, -122.4394472), (37.7985528, -122.4059284), (37.7872457, -122.4187461), (37.7674741, -122.4285306), (37.7861649, -122.414424), (37.78718629999999, -122.4192291), (37.7850378, -122.4171045), (37.8858697, -122.4677895), (37.7892932, -122.4123523), (37.7866819, -122.4684794), (37.7850687, -122.4164193), (37.7858422, -122.4185638), (37.7598648, -122.4147977), (37.7858422, -122.4185638), (37.789398, -122.4127312), (37.7907831, -122.4094509), (37.786295, -122.414992), (37.7858302, -122.4015144), (37.7892932, -122.4123523), (37.75908654243, -122.38941810716), (37.7746817, -122.478998), (37.7847382, -122.4091996), (37.7852868, -122.41479), (37.7892932, -122.4123523), (37.7858422, -122.4185638), (37.777617400000004, -122.4261667), (37.780204000000005, -122.48834599999999), (37.7590587, -122.4484882), (37.7894761, -122.4159362), (37.7863965, -122.414493), (37.7590587, -122.4484882), (37.7828786, -122.4146017), (37.7759073, -122.4245247), (37.8015086, -122.4315256), (37.79992, -122.437463), (37.754507000000004, -122.44836799999999), (37.7992369, -122.446832), (37.731147, -122.453149), (37.774002, -122.466283), (37.7852205, -122.4159761), (37.792494, -122.4163244), (37.793014, -122.416113), (37.7852612, -122.414262), (37.7858422, -122.4185638), (37.7865715, -122.47096429999999), (37.794747, -122.425952), (37.7829257, -122.4109508), (37.7892932, -122.4123523), (37.7892932, -122.4123523), (37.7925153, -122.4382307), (37.7829257, -122.4109508), (37.7843657, -122.4140903), (37.7877909, -122.4249523), (37.786295, -122.414992), (37.797885300000004, -122.43010179999999), (37.773426, -122.444885), (37.7598648, -122.4147977), (37.785919391957, -122.49053728829), (37.7847382, -122.4091996), (37.7847916, -122.4148535), (37.783117, -122.416765), (37.7968561, -122.4428665), (37.787045, -122.415948), (37.7852612, -122.414262), (39.028124733101,	-74.927615618332), (37.7852612, -122.414262), (37.784441, -122.48967599999999), (37.7673809, -122.4313266), (37.76467710000001, -122.4195644), (37.786862, -122.414395), (37.75514690000001, -122.4185591), (37.799696, -122.423818), (37.7674741, -122.4285306), (37.79874909999999, -122.4237074), (37.7925153, -122.4382307), (37.801263824305, -122.40609791445), (37.7987396, -122.3998979), (37.7422603, -122.4977355), (37.7806303, -122.3905427), (37.784035108661, -122.45369703508), (37.7633011, -122.4806025), (37.78660110000001, -122.3991105), (37.7892932, -122.4123523), (37.786862, -122.414395), (37.65750209999999, -122.4145629), (37.7679478, -122.4197387), (37.7925153, -122.4382307), (37.7850687, -122.4164193), (37.7894761, -122.4159362), (37.7829257, -122.4109508), (37.778516255255, -122.40562101174), (37.789398, -122.4127312), (37.7829257, -122.4109508), (37.7850378, -122.4171045), (37.7600053, -122.41097459999999), (37.7939885, -122.4298258), (37.78203269999999, -122.4689625), (37.7847916, -122.4148535), (37.7852205, -122.4159761), (37.8036235, -122.4063514), (37.7841269, -122.4167064), (37.7925153, -122.4382307), (37.7869108, -122.4134283), (37.790379, -122.4086003), (37.7861649, -122.414424), (37.7418326, -122.4291039), (37.7964623, -122.4166463), (37.775908785269, -122.42452517712), (37.7852868, -122.41479), (37.7872457, -122.4187461), (37.7894761, -122.4159362), (37.7831066, -122.4136744), (37.7831066, -122.4136744), (37.7925153, -122.4382307), (37.78609489999999, -122.4195711), (37.7940251, -122.4142205), (37.794846, -122.412787), (37.7852205, -122.4159761), (37.7925153, -122.4382307), (37.888634, -122.268172), (37.7925153, -122.4382307), (37.787655546972, -122.4437547166), (37.794165400000004, -122.412718), (37.7642205, -122.3974366), (37.74178089999999, -122.423328), (37.770013, -122.423497), (37.7873556, -122.4234272), (37.789398, -122.4127312), (37.7697983, -122.447138), (37.790362, -122.416889), (37.78561699999999, -122.412155), (37.7909514, -122.4078679), (37.8012389, -122.258299), (37.783117, -122.416765), (37.790826, -122.413322), (37.793232, -122.4144885), (37.7889515, -122.4121875), (37.7892932, -122.4123523), (37.7772419, -122.43301059999999), (37.7846203, -122.4169635), (37.7911417, -122.4376241), (37.7957315, -122.4463092), (37.7864829, -122.4169208), (37.7846203, -122.4169635), (37.7923795, -122.4082628), (37.803831, -122.408549), (37.798253, -122.4013607), (37.7892932, -122.4123523), (37.7879082, -122.4130443), (37.7641316, -122.4633656), (37.75740589999999, -122.427721), (37.7836888, -122.489301), (37.7831066, -122.4136744), (37.790379, -122.4086003), (37.7935328, -122.4207558), (37.7892932, -122.4123523), (37.73156, -122.46722799999999), (37.7892932, -122.4123523), (37.787045, -122.415948), (37.7931482, -122.4163966), (37.7850687, -122.4164193), (37.786295, -122.414992), (37.7861649, -122.414424), (37.7863965, -122.414493), (37.786295, -122.414992), (37.7786104, -122.4226408), (37.7590733, -122.4290386), (37.7850687, -122.4164193), (37.763914, -122.403766), (37.799048, -122.4464), (37.788368, -122.42812), (37.774773, -122.427361), (37.799048, -122.4464), (37.7923996, -122.4080166), (37.799048, -122.4464), (37.786295, -122.414992), (37.7937362, -122.424713), (37.7859731, -122.4551073), (37.865245, -122.506402), (37.7850378, -122.4171045), (37.8019913, -122.4486565), (37.7801477, -122.4731219), (37.8024681, -122.4216836), (37.6791106, -122.4792762), (37.8010963, -122.4195558), (37.8024681, -122.4216836), (47.6134351,	-101.4262561), (37.799754161949, -122.43743421717), (37.7981684, -122.4394472), (37.782282800000004, -122.4749561), (37.7909667, -122.4086836), (37.7981684, -122.4394472), (37.8782799, -122.295497), (37.792013, -122.4077845), (37.74300059999999, -122.495836), (37.79518849999999, -122.4128712), (37.759374, -122.442575), (37.78291979999999, -122.4078691), (37.7647261, -122.4354578), (37.794976, -122.427243), (37.8010963, -122.4195558), (37.783117, -122.416765), (37.7986559, -122.4307641), (37.798912, -122.41586099999999), (37.789398, -122.4127312), (37.7850378, -122.4171045), (37.8010963, -122.4195558), (37.7923795, -122.4082628), (37.8010963, -122.4195558), (37.7805641, -122.4866404), (37.79230829999999, -122.4430274), (37.7863965, -122.414493), (37.79017169999999, -122.4088161), (37.779902510647, -122.46420189726), (37.7805641, -122.4866404), (37.7858302, -122.4015144), (37.794976, -122.427243), (37.786862, -122.414395), (37.7638058, -122.4780194), (37.792051, -122.4186723), (37.8010963, -122.4195558), (37.7847916, -122.4148535), (37.7841269, -122.4167064), (37.7892932, -122.4123523), (37.78718629999999, -122.4192291), (37.793232, -122.4144885), (37.8006843, -122.444155), (37.7858422, -122.4185638), (37.7894761, -122.4159362), (37.798912, -122.41586099999999), (37.781664, -122.44335799999999), (37.7879155, -122.4174338), (37.7852612, -122.414262), (37.7850687, -122.4164193), (37.8097154, -122.2430141), (37.7841269, -122.4167064), (37.7828786, -122.4146017), (37.775908785269, -122.42452517712), (37.7926526, -122.4185175), (37.782407, -122.415263), (37.7847382, -122.4091996), (37.7947025, -122.4316546), (37.7589605, -122.41952309999999), (37.761778, -122.42864), (37.7801477, -122.4731219), (37.7805641, -122.4866404), (37.7847382, -122.4091996), (37.7863422, -122.4789997), (37.8004409, -122.4243087), (37.775908785269, -122.42452517712), (37.7877909, -122.4249523), (37.787045, -122.415948), (37.7863965, -122.414493), (37.799699, -122.4225433), (37.7831066, -122.4136744), (37.798053, -122.4197241), (37.784035108661, -122.45369703508), (37.7852205, -122.4159761), (37.7798704, -122.4647458), (37.8025002, -122.423104), (37.778341, -122.3965525), (37.7926526, -122.4185175), (37.7864829, -122.4169208), (37.800594338075, -122.43445181015), (37.7872457, -122.4187461), (37.8412557, -122.2058856), (37.78718629999999, -122.4192291), (37.7683467, -122.4206031), (37.789398, -122.4127312), (37.7843657, -122.4140903), (37.786862, -122.414395), (37.7786104, -122.4226408), (37.790379, -122.4086003), (37.7861649, -122.414424), (37.7858422, -122.4185638), (37.7850378, -122.4171045), (37.7889515, -122.4121875), (37.7639983, -122.3891506), (37.7926526, -122.4185175), (37.7894761, -122.4159362), (37.787683, -122.3917907), (37.7889515, -122.4121875), (37.751779500000005, -122.4215667), (37.8010963, -122.4195558), (37.758424000000005, -122.43182), (37.7867735, -122.4241362), (37.7852205, -122.4159761), (37.775357500000005, -122.4072966), (37.7598648, -122.4147977), (40.7074719,	-74.0112173), (37.801737, -122.423301), (37.796648, -122.4104941), (37.7996912, -122.4299847), (37.7927193, -122.4395637), (37.797407, -122.4313555), (37.7930123, -122.4153147), (37.7852868, -122.41479), (37.8030429, -122.4230169), (37.779931707732, -122.46423859482), (37.7881209, -122.3954958), (37.8018793, -122.4166171), (37.768268, -122.423742), (37.738425, -122.4338796), (37.7618282, -122.4313465), (37.744458, -122.423372), (37.7172769, -122.4651228), (37.775043, -122.4124792), (37.775928, -122.397528), (37.789553, -122.4123553), (37.7718376, -122.3886103), (37.77393, -122.396241), (37.7964731, -122.4289351), (37.76898800000001, -122.420347), (37.7961146, -122.4031229), (37.7705413, -122.1922076), (37.7375477, -122.438104), (37.771075341909, -122.43873411924), (37.7817846, -122.4208606), (37.7808916, -122.2052078), (37.8004667, -122.4221462), (37.6670901, -122.1140974), (37.8036401, -122.2623468), (37.7705413, -122.1922076), (37.7619757, -122.1834843), (37.836085, -122.2735944), (37.8084703, -122.2454161), (37.7850378, -122.4171045), (37.793435, -122.414172), (37.7758614, -122.4385771), (37.7808916, -122.2052078), (37.7835826, -122.3899066), (37.777463, -122.4131762), (37.764297, -122.445007), (37.7598648, -122.4147977), (37.7589982, -122.4177836), (37.7598648, -122.4147977), (37.7758689, -122.4297477), (37.7598648, -122.4147977), (37.7598648, -122.4147977), (37.7597727, -122.427063), (37.789398, -122.4127312), (37.911673473181, -122.37279839196), (37.7604987, -122.4283292), (37.784034046346, -122.48519248302), (37.9112002, -122.3756084), (37.7638058, -122.4780194), (37.772836, -122.426967), (37.7357931, -122.4201878), (37.7943276, -122.4149636), (37.911673473181, -122.37279839196), (37.911673473181, -122.37279839196), (37.788253600000004, -122.40308139999999), (37.760440200000005, -122.4513405), (37.775908785269, -122.42452517712), (37.7749295, -122.4194155), (37.788253600000004, -122.40308139999999), (37.7821433, -122.4490548), (37.786214, -122.404305), (37.7639587, -122.4684798), (37.774363, -122.4294151), (37.797548821497, -122.25509283169), (37.7926526, -122.4185175), (37.790379, -122.4086003), (37.779840703473, -122.46423178886), (37.7829257, -122.4109508), (37.789398, -122.4127312), (37.8030664, -122.4281232), (37.793435, -122.414172), (37.78718629999999, -122.4192291), (37.9499307, -122.5596903), (37.7850378, -122.4171045), (37.7847916, -122.4148535), (37.7858422, -122.4185638), (37.7841497, -122.4538009), (37.7864829, -122.4169208), (37.7925153, -122.4382307), (37.789622, -122.4151933), (37.7892932, -122.4123523), (37.7831066, -122.4136744), (37.7632646521, -122.45783976), (37.7925153, -122.4382307), (37.7831066, -122.4136744), (37.7861649, -122.414424), (37.7890394, -122.413467), (37.7849159, -122.4410273), (37.790379, -122.4086003), (37.778341, -122.3965525), (37.75037549268, -122.43384596963), (37.7749912, -122.4444032), (37.7686074, -122.4265363), (37.7780083, -122.3917215), (37.740567, -122.42917), (37.7893013, -122.4177374), (37.7858422, -122.4185638), (37.784035108661, -122.45369703508), (37.7841269, -122.4167064), (37.7696523, -122.4295781), (37.789398, -122.4127312), (37.7852868, -122.41479), (37.7861649, -122.414424), (37.7893013, -122.4177374), (37.7894761, -122.4159362), (37.789398, -122.4127312), (37.7890394, -122.413467), (37.792986866492, -122.41611381002), (37.7894761, -122.4159362), (37.7923795, -122.4082628), (37.793435, -122.414172), (37.7858422, -122.4185638), (37.7649026, -122.4150086), (37.7872457, -122.4187461), (37.7898025, -122.4097813), (37.7868332, -122.4147438), (37.7776899, -122.4329883), (37.789398, -122.4127312), (37.7852868, -122.41479), (37.7889515, -122.4121875), (37.7786296, -122.422483), (37.74020813207, -122.49440378124), (37.7907688, -122.4095738), (37.793435, -122.414172), (37.7863965, -122.414493), (37.7852612, -122.414262), (37.747619900000004, -122.4758021), (37.789622, -122.4151933), (37.773017, -122.4018915), (37.7839007, -122.4184699), (37.7931482, -122.4163966), (37.7877909, -122.4249523), (37.7894078, -122.4131309), (37.793435, -122.414172), (37.789398, -122.4127312), (37.8030257, -122.4252745), (37.7889515, -122.4121875), (37.789622, -122.4151933), (37.7858422, -122.4185638), (37.7894078, -122.4131309), (37.7809496, -122.4168223), (37.789398, -122.4127312), (37.7879155, -122.4174338), (37.7889515, -122.4121875), (37.7850378, -122.4171045), (37.78619, -122.415158), (37.7825013, -122.4495738), (37.7923795, -122.4082628)]
