/*!
 * passwordly 0.0.1 (2014-01-07)
 * Copyright 2013-2014 Josh Yudaken
 * ---
 * JavaScript BigInteger library version 0.9
 * http://silentmatt.com/biginteger/
 * 
 * Copyright (c) 2009 Matthew Crumley <email@matthewcrumley.com>
 * Copyright (c) 2010,2011 by John Tobey <John.Tobey@gmail.com>
 * Licensed under the MIT license.
 * 
 * Support for arbitrary internal representation base was added by Vitaly Magerya.
 * ---
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-256, as defined
 * in FIPS 180-2
 * Version 2.2 Copyright Angel Marin, Paul Johnston 2000 - 2009.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * ---
 * jsBCrypt is an implementation of BCrypt written in JavaScript.
 * It uses Components of the ISAAC.
 * It is based upon jBCrypt
 * Licence: http://opensource.org/licenses/BSD-3-Clause
 * ---
 * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message
 * Digest Algorithm, as defined in RFC 1321.
 * Version 2.2 Copyright (C) Paul Johnston 1999 - 2009
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for more info.
 */

(function(exports) {
function bCrypt() {
	this.GENSALT_DEFAULT_LOG2_ROUNDS = 10;
	this.BCRYPT_SALT_LEN = 16;
	this.BLOWFISH_NUM_ROUNDS = 16;
	this.MAX_EXECUTION_TIME = 100;
	this.P_orig = [0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344, 0xa4093822,
			0x299f31d0, 0x082efa98, 0xec4e6c89, 0x452821e6, 0x38d01377,
			0xbe5466cf, 0x34e90c6c, 0xc0ac29b7, 0xc97c50dd, 0x3f84d5b5,
			0xb5470917, 0x9216d5d9, 0x8979fb1b];
	this.S_orig = [0xd1310ba6, 0x98dfb5ac, 0x2ffd72db, 0xd01adfb7, 0xb8e1afed,
			0x6a267e96, 0xba7c9045, 0xf12c7f99, 0x24a19947, 0xb3916cf7,
			0x0801f2e2, 0x858efc16, 0x636920d8, 0x71574e69, 0xa458fea3,
			0xf4933d7e, 0x0d95748f, 0x728eb658, 0x718bcd58, 0x82154aee,
			0x7b54a41d, 0xc25a59b5, 0x9c30d539, 0x2af26013, 0xc5d1b023,
			0x286085f0, 0xca417918, 0xb8db38ef, 0x8e79dcb0, 0x603a180e,
			0x6c9e0e8b, 0xb01e8a3e, 0xd71577c1, 0xbd314b27, 0x78af2fda,
			0x55605c60, 0xe65525f3, 0xaa55ab94, 0x57489862, 0x63e81440,
			0x55ca396a, 0x2aab10b6, 0xb4cc5c34, 0x1141e8ce, 0xa15486af,
			0x7c72e993, 0xb3ee1411, 0x636fbc2a, 0x2ba9c55d, 0x741831f6,
			0xce5c3e16, 0x9b87931e, 0xafd6ba33, 0x6c24cf5c, 0x7a325381,
			0x28958677, 0x3b8f4898, 0x6b4bb9af, 0xc4bfe81b, 0x66282193,
			0x61d809cc, 0xfb21a991, 0x487cac60, 0x5dec8032, 0xef845d5d,
			0xe98575b1, 0xdc262302, 0xeb651b88, 0x23893e81, 0xd396acc5,
			0x0f6d6ff3, 0x83f44239, 0x2e0b4482, 0xa4842004, 0x69c8f04a,
			0x9e1f9b5e, 0x21c66842, 0xf6e96c9a, 0x670c9c61, 0xabd388f0,
			0x6a51a0d2, 0xd8542f68, 0x960fa728, 0xab5133a3, 0x6eef0b6c,
			0x137a3be4, 0xba3bf050, 0x7efb2a98, 0xa1f1651d, 0x39af0176,
			0x66ca593e, 0x82430e88, 0x8cee8619, 0x456f9fb4, 0x7d84a5c3,
			0x3b8b5ebe, 0xe06f75d8, 0x85c12073, 0x401a449f, 0x56c16aa6,
			0x4ed3aa62, 0x363f7706, 0x1bfedf72, 0x429b023d, 0x37d0d724,
			0xd00a1248, 0xdb0fead3, 0x49f1c09b, 0x075372c9, 0x80991b7b,
			0x25d479d8, 0xf6e8def7, 0xe3fe501a, 0xb6794c3b, 0x976ce0bd,
			0x04c006ba, 0xc1a94fb6, 0x409f60c4, 0x5e5c9ec2, 0x196a2463,
			0x68fb6faf, 0x3e6c53b5, 0x1339b2eb, 0x3b52ec6f, 0x6dfc511f,
			0x9b30952c, 0xcc814544, 0xaf5ebd09, 0xbee3d004, 0xde334afd,
			0x660f2807, 0x192e4bb3, 0xc0cba857, 0x45c8740f, 0xd20b5f39,
			0xb9d3fbdb, 0x5579c0bd, 0x1a60320a, 0xd6a100c6, 0x402c7279,
			0x679f25fe, 0xfb1fa3cc, 0x8ea5e9f8, 0xdb3222f8, 0x3c7516df,
			0xfd616b15, 0x2f501ec8, 0xad0552ab, 0x323db5fa, 0xfd238760,
			0x53317b48, 0x3e00df82, 0x9e5c57bb, 0xca6f8ca0, 0x1a87562e,
			0xdf1769db, 0xd542a8f6, 0x287effc3, 0xac6732c6, 0x8c4f5573,
			0x695b27b0, 0xbbca58c8, 0xe1ffa35d, 0xb8f011a0, 0x10fa3d98,
			0xfd2183b8, 0x4afcb56c, 0x2dd1d35b, 0x9a53e479, 0xb6f84565,
			0xd28e49bc, 0x4bfb9790, 0xe1ddf2da, 0xa4cb7e33, 0x62fb1341,
			0xcee4c6e8, 0xef20cada, 0x36774c01, 0xd07e9efe, 0x2bf11fb4,
			0x95dbda4d, 0xae909198, 0xeaad8e71, 0x6b93d5a0, 0xd08ed1d0,
			0xafc725e0, 0x8e3c5b2f, 0x8e7594b7, 0x8ff6e2fb, 0xf2122b64,
			0x8888b812, 0x900df01c, 0x4fad5ea0, 0x688fc31c, 0xd1cff191,
			0xb3a8c1ad, 0x2f2f2218, 0xbe0e1777, 0xea752dfe, 0x8b021fa1,
			0xe5a0cc0f, 0xb56f74e8, 0x18acf3d6, 0xce89e299, 0xb4a84fe0,
			0xfd13e0b7, 0x7cc43b81, 0xd2ada8d9, 0x165fa266, 0x80957705,
			0x93cc7314, 0x211a1477, 0xe6ad2065, 0x77b5fa86, 0xc75442f5,
			0xfb9d35cf, 0xebcdaf0c, 0x7b3e89a0, 0xd6411bd3, 0xae1e7e49,
			0x00250e2d, 0x2071b35e, 0x226800bb, 0x57b8e0af, 0x2464369b,
			0xf009b91e, 0x5563911d, 0x59dfa6aa, 0x78c14389, 0xd95a537f,
			0x207d5ba2, 0x02e5b9c5, 0x83260376, 0x6295cfa9, 0x11c81968,
			0x4e734a41, 0xb3472dca, 0x7b14a94a, 0x1b510052, 0x9a532915,
			0xd60f573f, 0xbc9bc6e4, 0x2b60a476, 0x81e67400, 0x08ba6fb5,
			0x571be91f, 0xf296ec6b, 0x2a0dd915, 0xb6636521, 0xe7b9f9b6,
			0xff34052e, 0xc5855664, 0x53b02d5d, 0xa99f8fa1, 0x08ba4799,
			0x6e85076a, 0x4b7a70e9, 0xb5b32944, 0xdb75092e, 0xc4192623,
			0xad6ea6b0, 0x49a7df7d, 0x9cee60b8, 0x8fedb266, 0xecaa8c71,
			0x699a17ff, 0x5664526c, 0xc2b19ee1, 0x193602a5, 0x75094c29,
			0xa0591340, 0xe4183a3e, 0x3f54989a, 0x5b429d65, 0x6b8fe4d6,
			0x99f73fd6, 0xa1d29c07, 0xefe830f5, 0x4d2d38e6, 0xf0255dc1,
			0x4cdd2086, 0x8470eb26, 0x6382e9c6, 0x021ecc5e, 0x09686b3f,
			0x3ebaefc9, 0x3c971814, 0x6b6a70a1, 0x687f3584, 0x52a0e286,
			0xb79c5305, 0xaa500737, 0x3e07841c, 0x7fdeae5c, 0x8e7d44ec,
			0x5716f2b8, 0xb03ada37, 0xf0500c0d, 0xf01c1f04, 0x0200b3ff,
			0xae0cf51a, 0x3cb574b2, 0x25837a58, 0xdc0921bd, 0xd19113f9,
			0x7ca92ff6, 0x94324773, 0x22f54701, 0x3ae5e581, 0x37c2dadc,
			0xc8b57634, 0x9af3dda7, 0xa9446146, 0x0fd0030e, 0xecc8c73e,
			0xa4751e41, 0xe238cd99, 0x3bea0e2f, 0x3280bba1, 0x183eb331,
			0x4e548b38, 0x4f6db908, 0x6f420d03, 0xf60a04bf, 0x2cb81290,
			0x24977c79, 0x5679b072, 0xbcaf89af, 0xde9a771f, 0xd9930810,
			0xb38bae12, 0xdccf3f2e, 0x5512721f, 0x2e6b7124, 0x501adde6,
			0x9f84cd87, 0x7a584718, 0x7408da17, 0xbc9f9abc, 0xe94b7d8c,
			0xec7aec3a, 0xdb851dfa, 0x63094366, 0xc464c3d2, 0xef1c1847,
			0x3215d908, 0xdd433b37, 0x24c2ba16, 0x12a14d43, 0x2a65c451,
			0x50940002, 0x133ae4dd, 0x71dff89e, 0x10314e55, 0x81ac77d6,
			0x5f11199b, 0x043556f1, 0xd7a3c76b, 0x3c11183b, 0x5924a509,
			0xf28fe6ed, 0x97f1fbfa, 0x9ebabf2c, 0x1e153c6e, 0x86e34570,
			0xeae96fb1, 0x860e5e0a, 0x5a3e2ab3, 0x771fe71c, 0x4e3d06fa,
			0x2965dcb9, 0x99e71d0f, 0x803e89d6, 0x5266c825, 0x2e4cc978,
			0x9c10b36a, 0xc6150eba, 0x94e2ea78, 0xa5fc3c53, 0x1e0a2df4,
			0xf2f74ea7, 0x361d2b3d, 0x1939260f, 0x19c27960, 0x5223a708,
			0xf71312b6, 0xebadfe6e, 0xeac31f66, 0xe3bc4595, 0xa67bc883,
			0xb17f37d1, 0x018cff28, 0xc332ddef, 0xbe6c5aa5, 0x65582185,
			0x68ab9802, 0xeecea50f, 0xdb2f953b, 0x2aef7dad, 0x5b6e2f84,
			0x1521b628, 0x29076170, 0xecdd4775, 0x619f1510, 0x13cca830,
			0xeb61bd96, 0x0334fe1e, 0xaa0363cf, 0xb5735c90, 0x4c70a239,
			0xd59e9e0b, 0xcbaade14, 0xeecc86bc, 0x60622ca7, 0x9cab5cab,
			0xb2f3846e, 0x648b1eaf, 0x19bdf0ca, 0xa02369b9, 0x655abb50,
			0x40685a32, 0x3c2ab4b3, 0x319ee9d5, 0xc021b8f7, 0x9b540b19,
			0x875fa099, 0x95f7997e, 0x623d7da8, 0xf837889a, 0x97e32d77,
			0x11ed935f, 0x16681281, 0x0e358829, 0xc7e61fd6, 0x96dedfa1,
			0x7858ba99, 0x57f584a5, 0x1b227263, 0x9b83c3ff, 0x1ac24696,
			0xcdb30aeb, 0x532e3054, 0x8fd948e4, 0x6dbc3128, 0x58ebf2ef,
			0x34c6ffea, 0xfe28ed61, 0xee7c3c73, 0x5d4a14d9, 0xe864b7e3,
			0x42105d14, 0x203e13e0, 0x45eee2b6, 0xa3aaabea, 0xdb6c4f15,
			0xfacb4fd0, 0xc742f442, 0xef6abbb5, 0x654f3b1d, 0x41cd2105,
			0xd81e799e, 0x86854dc7, 0xe44b476a, 0x3d816250, 0xcf62a1f2,
			0x5b8d2646, 0xfc8883a0, 0xc1c7b6a3, 0x7f1524c3, 0x69cb7492,
			0x47848a0b, 0x5692b285, 0x095bbf00, 0xad19489d, 0x1462b174,
			0x23820e00, 0x58428d2a, 0x0c55f5ea, 0x1dadf43e, 0x233f7061,
			0x3372f092, 0x8d937e41, 0xd65fecf1, 0x6c223bdb, 0x7cde3759,
			0xcbee7460, 0x4085f2a7, 0xce77326e, 0xa6078084, 0x19f8509e,
			0xe8efd855, 0x61d99735, 0xa969a7aa, 0xc50c06c2, 0x5a04abfc,
			0x800bcadc, 0x9e447a2e, 0xc3453484, 0xfdd56705, 0x0e1e9ec9,
			0xdb73dbd3, 0x105588cd, 0x675fda79, 0xe3674340, 0xc5c43465,
			0x713e38d8, 0x3d28f89e, 0xf16dff20, 0x153e21e7, 0x8fb03d4a,
			0xe6e39f2b, 0xdb83adf7, 0xe93d5a68, 0x948140f7, 0xf64c261c,
			0x94692934, 0x411520f7, 0x7602d4f7, 0xbcf46b2e, 0xd4a20068,
			0xd4082471, 0x3320f46a, 0x43b7d4b7, 0x500061af, 0x1e39f62e,
			0x97244546, 0x14214f74, 0xbf8b8840, 0x4d95fc1d, 0x96b591af,
			0x70f4ddd3, 0x66a02f45, 0xbfbc09ec, 0x03bd9785, 0x7fac6dd0,
			0x31cb8504, 0x96eb27b3, 0x55fd3941, 0xda2547e6, 0xabca0a9a,
			0x28507825, 0x530429f4, 0x0a2c86da, 0xe9b66dfb, 0x68dc1462,
			0xd7486900, 0x680ec0a4, 0x27a18dee, 0x4f3ffea2, 0xe887ad8c,
			0xb58ce006, 0x7af4d6b6, 0xaace1e7c, 0xd3375fec, 0xce78a399,
			0x406b2a42, 0x20fe9e35, 0xd9f385b9, 0xee39d7ab, 0x3b124e8b,
			0x1dc9faf7, 0x4b6d1856, 0x26a36631, 0xeae397b2, 0x3a6efa74,
			0xdd5b4332, 0x6841e7f7, 0xca7820fb, 0xfb0af54e, 0xd8feb397,
			0x454056ac, 0xba489527, 0x55533a3a, 0x20838d87, 0xfe6ba9b7,
			0xd096954b, 0x55a867bc, 0xa1159a58, 0xcca92963, 0x99e1db33,
			0xa62a4a56, 0x3f3125f9, 0x5ef47e1c, 0x9029317c, 0xfdf8e802,
			0x04272f70, 0x80bb155c, 0x05282ce3, 0x95c11548, 0xe4c66d22,
			0x48c1133f, 0xc70f86dc, 0x07f9c9ee, 0x41041f0f, 0x404779a4,
			0x5d886e17, 0x325f51eb, 0xd59bc0d1, 0xf2bcc18f, 0x41113564,
			0x257b7834, 0x602a9c60, 0xdff8e8a3, 0x1f636c1b, 0x0e12b4c2,
			0x02e1329e, 0xaf664fd1, 0xcad18115, 0x6b2395e0, 0x333e92e1,
			0x3b240b62, 0xeebeb922, 0x85b2a20e, 0xe6ba0d99, 0xde720c8c,
			0x2da2f728, 0xd0127845, 0x95b794fd, 0x647d0862, 0xe7ccf5f0,
			0x5449a36f, 0x877d48fa, 0xc39dfd27, 0xf33e8d1e, 0x0a476341,
			0x992eff74, 0x3a6f6eab, 0xf4f8fd37, 0xa812dc60, 0xa1ebddf8,
			0x991be14c, 0xdb6e6b0d, 0xc67b5510, 0x6d672c37, 0x2765d43b,
			0xdcd0e804, 0xf1290dc7, 0xcc00ffa3, 0xb5390f92, 0x690fed0b,
			0x667b9ffb, 0xcedb7d9c, 0xa091cf0b, 0xd9155ea3, 0xbb132f88,
			0x515bad24, 0x7b9479bf, 0x763bd6eb, 0x37392eb3, 0xcc115979,
			0x8026e297, 0xf42e312d, 0x6842ada7, 0xc66a2b3b, 0x12754ccc,
			0x782ef11c, 0x6a124237, 0xb79251e7, 0x06a1bbe6, 0x4bfb6350,
			0x1a6b1018, 0x11caedfa, 0x3d25bdd8, 0xe2e1c3c9, 0x44421659,
			0x0a121386, 0xd90cec6e, 0xd5abea2a, 0x64af674e, 0xda86a85f,
			0xbebfe988, 0x64e4c3fe, 0x9dbc8057, 0xf0f7c086, 0x60787bf8,
			0x6003604d, 0xd1fd8346, 0xf6381fb0, 0x7745ae04, 0xd736fccc,
			0x83426b33, 0xf01eab71, 0xb0804187, 0x3c005e5f, 0x77a057be,
			0xbde8ae24, 0x55464299, 0xbf582e61, 0x4e58f48f, 0xf2ddfda2,
			0xf474ef38, 0x8789bdc2, 0x5366f9c3, 0xc8b38e74, 0xb475f255,
			0x46fcd9b9, 0x7aeb2661, 0x8b1ddf84, 0x846a0e79, 0x915f95e2,
			0x466e598e, 0x20b45770, 0x8cd55591, 0xc902de4c, 0xb90bace1,
			0xbb8205d0, 0x11a86248, 0x7574a99e, 0xb77f19b6, 0xe0a9dc09,
			0x662d09a1, 0xc4324633, 0xe85a1f02, 0x09f0be8c, 0x4a99a025,
			0x1d6efe10, 0x1ab93d1d, 0x0ba5a4df, 0xa186f20f, 0x2868f169,
			0xdcb7da83, 0x573906fe, 0xa1e2ce9b, 0x4fcd7f52, 0x50115e01,
			0xa70683fa, 0xa002b5c4, 0x0de6d027, 0x9af88c27, 0x773f8641,
			0xc3604c06, 0x61a806b5, 0xf0177a28, 0xc0f586e0, 0x006058aa,
			0x30dc7d62, 0x11e69ed7, 0x2338ea63, 0x53c2dd94, 0xc2c21634,
			0xbbcbee56, 0x90bcb6de, 0xebfc7da1, 0xce591d76, 0x6f05e409,
			0x4b7c0188, 0x39720a3d, 0x7c927c24, 0x86e3725f, 0x724d9db9,
			0x1ac15bb4, 0xd39eb8fc, 0xed545578, 0x08fca5b5, 0xd83d7cd3,
			0x4dad0fc4, 0x1e50ef5e, 0xb161e6f8, 0xa28514d9, 0x6c51133c,
			0x6fd5c7e7, 0x56e14ec4, 0x362abfce, 0xddc6c837, 0xd79a3234,
			0x92638212, 0x670efa8e, 0x406000e0, 0x3a39ce37, 0xd3faf5cf,
			0xabc27737, 0x5ac52d1b, 0x5cb0679e, 0x4fa33742, 0xd3822740,
			0x99bc9bbe, 0xd5118e9d, 0xbf0f7315, 0xd62d1c7e, 0xc700c47b,
			0xb78c1b6b, 0x21a19045, 0xb26eb1be, 0x6a366eb4, 0x5748ab2f,
			0xbc946e79, 0xc6a376d2, 0x6549c2c8, 0x530ff8ee, 0x468dde7d,
			0xd5730a1d, 0x4cd04dc6, 0x2939bbdb, 0xa9ba4650, 0xac9526e8,
			0xbe5ee304, 0xa1fad5f0, 0x6a2d519a, 0x63ef8ce2, 0x9a86ee22,
			0xc089c2b8, 0x43242ef6, 0xa51e03aa, 0x9cf2d0a4, 0x83c061ba,
			0x9be96a4d, 0x8fe51550, 0xba645bd6, 0x2826a2f9, 0xa73a3ae1,
			0x4ba99586, 0xef5562e9, 0xc72fefd3, 0xf752f7da, 0x3f046f69,
			0x77fa0a59, 0x80e4a915, 0x87b08601, 0x9b09e6ad, 0x3b3ee593,
			0xe990fd5a, 0x9e34d797, 0x2cf0b7d9, 0x022b8b51, 0x96d5ac3a,
			0x017da67d, 0xd1cf3ed6, 0x7c7d2d28, 0x1f9f25cf, 0xadf2b89b,
			0x5ad6b472, 0x5a88f54c, 0xe029ac71, 0xe019a5e6, 0x47b0acfd,
			0xed93fa9b, 0xe8d3c48d, 0x283b57cc, 0xf8d56629, 0x79132e28,
			0x785f0191, 0xed756055, 0xf7960e44, 0xe3d35e8c, 0x15056dd4,
			0x88f46dba, 0x03a16125, 0x0564f0bd, 0xc3eb9e15, 0x3c9057a2,
			0x97271aec, 0xa93a072a, 0x1b3f6d9b, 0x1e6321f5, 0xf59c66fb,
			0x26dcf319, 0x7533d928, 0xb155fdf5, 0x03563482, 0x8aba3cbb,
			0x28517711, 0xc20ad9f8, 0xabcc5167, 0xccad925f, 0x4de81751,
			0x3830dc8e, 0x379d5862, 0x9320f991, 0xea7a90c2, 0xfb3e7bce,
			0x5121ce64, 0x774fbe32, 0xa8b6e37e, 0xc3293d46, 0x48de5369,
			0x6413e680, 0xa2ae0810, 0xdd6db224, 0x69852dfd, 0x09072166,
			0xb39a460a, 0x6445c0dd, 0x586cdecf, 0x1c20c8ae, 0x5bbef7dd,
			0x1b588d40, 0xccd2017f, 0x6bb4e3bb, 0xdda26a7e, 0x3a59ff45,
			0x3e350a44, 0xbcb4cdd5, 0x72eacea8, 0xfa6484bb, 0x8d6612ae,
			0xbf3c6f47, 0xd29be463, 0x542f5d9e, 0xaec2771b, 0xf64e6370,
			0x740e0d8d, 0xe75b1357, 0xf8721671, 0xaf537d5d, 0x4040cb08,
			0x4eb4e2cc, 0x34d2466a, 0x0115af84, 0xe1b00428, 0x95983a1d,
			0x06b89fb4, 0xce6ea048, 0x6f3f3b82, 0x3520ab82, 0x011a1d4b,
			0x277227f8, 0x611560b1, 0xe7933fdc, 0xbb3a792b, 0x344525bd,
			0xa08839e1, 0x51ce794b, 0x2f32c9b7, 0xa01fbac9, 0xe01cc87e,
			0xbcc7d1f6, 0xcf0111c3, 0xa1e8aac7, 0x1a908749, 0xd44fbd9a,
			0xd0dadecb, 0xd50ada38, 0x0339c32a, 0xc6913667, 0x8df9317c,
			0xe0b12b4f, 0xf79e59b7, 0x43f5bb3a, 0xf2d519ff, 0x27d9459c,
			0xbf97222c, 0x15e6fc2a, 0x0f91fc71, 0x9b941525, 0xfae59361,
			0xceb69ceb, 0xc2a86459, 0x12baa8d1, 0xb6c1075e, 0xe3056a0c,
			0x10d25065, 0xcb03a442, 0xe0ec6e0e, 0x1698db3b, 0x4c98a0be,
			0x3278e964, 0x9f1f9532, 0xe0d392df, 0xd3a0342b, 0x8971f21e,
			0x1b0a7441, 0x4ba3348c, 0xc5be7120, 0xc37632d8, 0xdf359f8d,
			0x9b992f2e, 0xe60b6f47, 0x0fe3f11d, 0xe54cda54, 0x1edad891,
			0xce6279cf, 0xcd3e7e6f, 0x1618b166, 0xfd2c1d05, 0x848fd2c5,
			0xf6fb2299, 0xf523f357, 0xa6327623, 0x93a83531, 0x56cccd02,
			0xacf08162, 0x5a75ebb5, 0x6e163697, 0x88d273cc, 0xde966292,
			0x81b949d0, 0x4c50901b, 0x71c65614, 0xe6c6c7bd, 0x327a140a,
			0x45e1d006, 0xc3f27b9a, 0xc9aa53fd, 0x62a80f00, 0xbb25bfe2,
			0x35bdd2f6, 0x71126905, 0xb2040222, 0xb6cbcf7c, 0xcd769c2b,
			0x53113ec0, 0x1640e3d3, 0x38abbd60, 0x2547adf0, 0xba38209c,
			0xf746ce76, 0x77afa1c5, 0x20756060, 0x85cbfe4e, 0x8ae88dd8,
			0x7aaaf9b0, 0x4cf9aa7e, 0x1948c25c, 0x02fb8a8c, 0x01c36ae4,
			0xd6ebe1f9, 0x90d4f869, 0xa65cdea0, 0x3f09252d, 0xc208e69f,
			0xb74e6132, 0xce77e25b, 0x578fdfe3, 0x3ac372e6];
	this.bf_crypt_ciphertext = [0x4f727068, 0x65616e42, 0x65686f6c, 0x64657253,
			0x63727944, 0x6f756274];
	this.base64_code = ['.', '/', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
			'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
			'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
			'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
			'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8',
			'9'];
	this.index_64 = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1,
			54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1, -1,
			2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
			21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, -1, 28, 29, 30, 31,
			32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
			49, 50, 51, 52, 53, -1, -1, -1, -1, -1];
	this.P;
	this.S;
	this.lr;
	this.offp;
};
bCrypt.prototype.getByte = function(c) {
	var ret = 0;
	try {
		var b = c.charCodeAt(0);
	} catch (err) {
		b = c;
	}
	if (b > 127) {
		return -128 + (b % 128);
	} else {
		return b;
	}
};
bCrypt.prototype.encode_base64 = function(d, len) {
	var off = 0;
	var rs = [];
	var c1;
	var c2;
	if (len <= 0 || len > d.length)
		throw "Invalid len";
	while (off < len) {
		c1 = d[off++] & 0xff;
		rs.push(this.base64_code[(c1 >> 2) & 0x3f]);
		c1 = (c1 & 0x03) << 4;
		if (off >= len) {
			rs.push(this.base64_code[c1 & 0x3f]);
			break;
		}
		c2 = d[off++] & 0xff;
		c1 |= (c2 >> 4) & 0x0f;
		rs.push(this.base64_code[c1 & 0x3f]);
		c1 = (c2 & 0x0f) << 2;
		if (off >= len) {
			rs.push(this.base64_code[c1 & 0x3f]);
			break;
		}
		c2 = d[off++] & 0xff;
		c1 |= (c2 >> 6) & 0x03;
		rs.push(this.base64_code[c1 & 0x3f]);
		rs.push(this.base64_code[c2 & 0x3f]);
	}
	return rs.join('');
};
bCrypt.prototype.char64 = function(x) {
	var code = x.charCodeAt(0);
	if (code < 0 || code > this.index_64.length) {
		return -1;
	}
	return this.index_64[code];
};
bCrypt.prototype.decode_base64 = function(s, maxolen) {
	var off = 0;
	var slen = s.length;
	var olen = 0;
	var rs = [];
	var c1, c2, c3, c4, o;
	if (maxolen <= 0)
		throw "Invalid maxolen";
	while (off < slen - 1 && olen < maxolen) {
		c1 = this.char64(s.charAt(off++));
		c2 = this.char64(s.charAt(off++));
		if (c1 == -1 || c2 == -1) {
			break;
		}
		o = this.getByte(c1 << 2);
		o |= (c2 & 0x30) >> 4;
		rs.push(String.fromCharCode(o));
		if (++olen >= maxolen || off >= slen) {
			break;
		}
		c3 = this.char64(s.charAt(off++));
		if (c3 == -1) {
			break;
		}
		o = this.getByte((c2 & 0x0f) << 4);
		o |= (c3 & 0x3c) >> 2;
		rs.push(String.fromCharCode(o));
		if (++olen >= maxolen || off >= slen) {
			break;
		}
		c4 = this.char64(s.charAt(off++));
		o = this.getByte((c3 & 0x03) << 6);
		o |= c4;
		rs.push(String.fromCharCode(o));
		++olen;
	}
	var ret = [];
	for (off = 0; off < olen; off++) {
		ret.push(this.getByte(rs[off]));
	}
	return ret;
};
bCrypt.prototype.encipher = function(lr, off) {
	var i;
	var n;
	var l = lr[off];
	var r = lr[off + 1];

	l ^= this.P[0];
	for (i = 0; i <= this.BLOWFISH_NUM_ROUNDS - 2;) {
		// Feistel substitution on left word
		n = this.S[(l >> 24) & 0xff];
		n += this.S[0x100 | ((l >> 16) & 0xff)];
		n ^= this.S[0x200 | ((l >> 8) & 0xff)];
		n += this.S[0x300 | (l & 0xff)];
		r ^= n ^ this.P[++i];

		// Feistel substitution on right word
		n = this.S[(r >> 24) & 0xff];
		n += this.S[0x100 | ((r >> 16) & 0xff)];
		n ^= this.S[0x200 | ((r >> 8) & 0xff)];
		n += this.S[0x300 | (r & 0xff)];
		l ^= n ^ this.P[++i];
	}
	lr[off] = r ^ this.P[this.BLOWFISH_NUM_ROUNDS + 1];
	lr[off + 1] = l;
};
bCrypt.prototype.streamtoword = function(data, offp) {
	var i;
	var word = 0;
	var off = offp;
	for (i = 0; i < 4; i++) {
		word = (word << 8) | (data[off] & 0xff);
		off = (off + 1) % data.length;
	}
	this.offp = off;
	return word;
};
bCrypt.prototype.init_key = function() {
	this.P = this.P_orig.slice();
	this.S = this.S_orig.slice();
};
bCrypt.prototype.key = function(key) {
	var i;
	this.offp = 0;
	var lr = new Array(0x00000000, 0x00000000);
	var plen = this.P.length;
	var slen = this.S.length;

	for (i = 0; i < plen; i++) {
		this.P[i] = this.P[i] ^ this.streamtoword(key, this.offp);
	}
	for (i = 0; i < plen; i += 2) {
		this.encipher(lr, 0);
		this.P[i] = lr[0];
		this.P[i + 1] = lr[1];
	}

	for (i = 0; i < slen; i += 2) {
		this.encipher(lr, 0);
		this.S[i] = lr[0];
		this.S[i + 1] = lr[1];
	}
};
bCrypt.prototype.ekskey = function(data, key) {
	var i;
	this.offp = 0;
	var lr = new Array(0x00000000, 0x00000000);
	var plen = this.P.length;
	var slen = this.S.length;

	for (i = 0; i < plen; i++)
		this.P[i] = this.P[i] ^ this.streamtoword(key, this.offp);
	this.offp = 0;
	for (i = 0; i < plen; i += 2) {
		lr[0] ^= this.streamtoword(data, this.offp);
		lr[1] ^= this.streamtoword(data, this.offp);
		this.encipher(lr, 0);
		this.P[i] = lr[0];
		this.P[i + 1] = lr[1];
	}
	for (i = 0; i < slen; i += 2) {
		lr[0] ^= this.streamtoword(data, this.offp);
		lr[1] ^= this.streamtoword(data, this.offp);
		this.encipher(lr, 0);
		this.S[i] = lr[0];
		this.S[i + 1] = lr[1];
	}
};

bCrypt.prototype.crypt_raw = function(password, salt, log_rounds, callback, progress) {
	var rounds;
	var j;
	var cdata = this.bf_crypt_ciphertext.slice();
	var clen = cdata.length;
	var one_percent;

	if (log_rounds < 4 || log_rounds > 31)
		throw "Bad number of rounds";
	if (salt.length != this.BCRYPT_SALT_LEN)
		throw "Bad salt length";

	rounds = 1 << log_rounds;
	one_percent = Math.floor(rounds / 100) + 1;
	this.init_key();
	this.ekskey(salt, password);

	var obj = this;
	var i = 0;
	setTimeout(function(){
		if(i < rounds){
			var start = new Date();
			for (; i < rounds;) {
				i = i + 1;
				obj.key(password);
				obj.key(salt);
		                if(i % one_percent == 0){
			        	progress();
                		}
		                if((new Date() - start) > obj.MAX_EXECUTION_TIME){
                    			break;
		                }
            		}
		        setTimeout(arguments.callee, 0);
        	}else{
 	        	for (i = 0; i < 64; i++) {
                		for (j = 0; j < (clen >> 1); j++) {
                    			obj.encipher(cdata, j << 1);
                		}
            		}
			var ret = [];
		        for (i = 0; i < clen; i++) {
                		ret.push(obj.getByte((cdata[i] >> 24) & 0xff));
                		ret.push(obj.getByte((cdata[i] >> 16) & 0xff));
                		ret.push(obj.getByte((cdata[i] >> 8) & 0xff));
                		ret.push(obj.getByte(cdata[i] & 0xff));
            		}
            		callback(ret);
        	}
    	}, 0);
};
/*
 * callback: a function that will be passed the hash when it is complete
 * progress: optional - this function will be called every time 1% of hashing
 *      is complete.
 */
bCrypt.prototype.hashpw = function(password, salt, callback, progress) {
	var real_salt;
	var passwordb = [];
	var saltb = [];
	var hashed = [];
	var minor = String.fromCharCode(0);
	var rounds = 0;
	var off = 0;

	if (!progress){
	        var progress = function() {};
	}

	if (salt.charAt(0) != '$' || salt.charAt(1) != '2')
		throw "Invalid salt version";
	if (salt.charAt(2) == '$')
		off = 3;
	else {
		minor = salt.charAt(2);
		if (minor != 'a' || salt.charAt(3) != '$')
			throw "Invalid salt revision";
		off = 4;
	}

	// Extract number of rounds
	if (salt.charAt(off + 2) > '$')
		throw "Missing salt rounds";
	var r1 = parseInt(salt.substring(off, off + 1)) * 10;
	var r2 = parseInt(salt.substring(off + 1, off + 2));
	rounds = r1 + r2;
	real_salt = salt.substring(off + 3, off + 25);
	password = password + (minor >= 'a' ? "\000" : "");
	for (var r = 0; r < password.length; r++) {
		passwordb.push(this.getByte(password.charAt(r)));
	}
	saltb = this.decode_base64(real_salt, this.BCRYPT_SALT_LEN);
	var obj = this;
	this.crypt_raw(passwordb, saltb, rounds, function(hashed) {
		var rs = [];
	        rs.push("$2");
	        if (minor >= 'a')
			rs.push(minor);
		rs.push("$");
        	if (rounds < 10)
			rs.push("0");
        	rs.push(rounds.toString());
	        rs.push("$");
	        rs.push(obj.encode_base64(saltb, saltb.length));
	        rs.push(obj.encode_base64(hashed, obj.bf_crypt_ciphertext.length * 4 - 1));
	        callback(rs.join(''));
	}, progress);
};

/*
	File: biginteger.js

	Exports:

		<BigInteger>
*/
(function(exports) {
"use strict";
/*
	Class: BigInteger
	An arbitrarily-large integer.

	<BigInteger> objects should be considered immutable. None of the "built-in"
	methods modify *this* or their arguments. All properties should be
	considered private.

	All the methods of <BigInteger> instances can be called "statically". The
	static versions are convenient if you don't already have a <BigInteger>
	object.

	As an example, these calls are equivalent.

	> BigInteger(4).multiply(5); // returns BigInteger(20);
	> BigInteger.multiply(4, 5); // returns BigInteger(20);

	> var a = 42;
	> var a = BigInteger.toJSValue("0b101010"); // Not completely useless...
*/

var CONSTRUCT = {}; // Unique token to call "private" version of constructor

/*
	Constructor: BigInteger()
	Convert a value to a <BigInteger>.

	Although <BigInteger()> is the constructor for <BigInteger> objects, it is
	best not to call it as a constructor. If *n* is a <BigInteger> object, it is
	simply returned as-is. Otherwise, <BigInteger()> is equivalent to <parse>
	without a radix argument.

	> var n0 = BigInteger();      // Same as <BigInteger.ZERO>
	> var n1 = BigInteger("123"); // Create a new <BigInteger> with value 123
	> var n2 = BigInteger(123);   // Create a new <BigInteger> with value 123
	> var n3 = BigInteger(n2);    // Return n2, unchanged

	The constructor form only takes an array and a sign. *n* must be an
	array of numbers in little-endian order, where each digit is between 0
	and BigInteger.base.  The second parameter sets the sign: -1 for
	negative, +1 for positive, or 0 for zero. The array is *not copied and
	may be modified*. If the array contains only zeros, the sign parameter
	is ignored and is forced to zero.

	> new BigInteger([5], -1): create a new BigInteger with value -5

	Parameters:

		n - Value to convert to a <BigInteger>.

	Returns:

		A <BigInteger> value.

	See Also:

		<parse>, <BigInteger>
*/
function BigInteger(n, s, token) {
	if (token !== CONSTRUCT) {
		if (n instanceof BigInteger) {
			return n;
		}
		else if (typeof n === "undefined") {
			return ZERO;
		}
		return BigInteger.parse(n);
	}

	n = n || [];  // Provide the nullary constructor for subclasses.
	while (n.length && !n[n.length - 1]) {
		--n.length;
	}
	this._d = n;
	this._s = n.length ? (s || 1) : 0;
}

BigInteger._construct = function(n, s) {
	return new BigInteger(n, s, CONSTRUCT);
};

// Base-10 speedup hacks in parse, toString, exp10 and log functions
// require base to be a power of 10. 10^7 is the largest such power
// that won't cause a precision loss when digits are multiplied.
var BigInteger_base = 10000000;
var BigInteger_base_log10 = 7;

BigInteger.base = BigInteger_base;
BigInteger.base_log10 = BigInteger_base_log10;

var ZERO = new BigInteger([], 0, CONSTRUCT);
// Constant: ZERO
// <BigInteger> 0.
BigInteger.ZERO = ZERO;

var ONE = new BigInteger([1], 1, CONSTRUCT);
// Constant: ONE
// <BigInteger> 1.
BigInteger.ONE = ONE;

var M_ONE = new BigInteger(ONE._d, -1, CONSTRUCT);
// Constant: M_ONE
// <BigInteger> -1.
BigInteger.M_ONE = M_ONE;

// Constant: _0
// Shortcut for <ZERO>.
BigInteger._0 = ZERO;

// Constant: _1
// Shortcut for <ONE>.
BigInteger._1 = ONE;

/*
	Constant: small
	Array of <BigIntegers> from 0 to 36.

	These are used internally for parsing, but useful when you need a "small"
	<BigInteger>.

	See Also:

		<ZERO>, <ONE>, <_0>, <_1>
*/
BigInteger.small = [
	ZERO,
	ONE,
	/* Assuming BigInteger_base > 36 */
	new BigInteger( [2], 1, CONSTRUCT),
	new BigInteger( [3], 1, CONSTRUCT),
	new BigInteger( [4], 1, CONSTRUCT),
	new BigInteger( [5], 1, CONSTRUCT),
	new BigInteger( [6], 1, CONSTRUCT),
	new BigInteger( [7], 1, CONSTRUCT),
	new BigInteger( [8], 1, CONSTRUCT),
	new BigInteger( [9], 1, CONSTRUCT),
	new BigInteger([10], 1, CONSTRUCT),
	new BigInteger([11], 1, CONSTRUCT),
	new BigInteger([12], 1, CONSTRUCT),
	new BigInteger([13], 1, CONSTRUCT),
	new BigInteger([14], 1, CONSTRUCT),
	new BigInteger([15], 1, CONSTRUCT),
	new BigInteger([16], 1, CONSTRUCT),
	new BigInteger([17], 1, CONSTRUCT),
	new BigInteger([18], 1, CONSTRUCT),
	new BigInteger([19], 1, CONSTRUCT),
	new BigInteger([20], 1, CONSTRUCT),
	new BigInteger([21], 1, CONSTRUCT),
	new BigInteger([22], 1, CONSTRUCT),
	new BigInteger([23], 1, CONSTRUCT),
	new BigInteger([24], 1, CONSTRUCT),
	new BigInteger([25], 1, CONSTRUCT),
	new BigInteger([26], 1, CONSTRUCT),
	new BigInteger([27], 1, CONSTRUCT),
	new BigInteger([28], 1, CONSTRUCT),
	new BigInteger([29], 1, CONSTRUCT),
	new BigInteger([30], 1, CONSTRUCT),
	new BigInteger([31], 1, CONSTRUCT),
	new BigInteger([32], 1, CONSTRUCT),
	new BigInteger([33], 1, CONSTRUCT),
	new BigInteger([34], 1, CONSTRUCT),
	new BigInteger([35], 1, CONSTRUCT),
	new BigInteger([36], 1, CONSTRUCT)
];

// Used for parsing/radix conversion
BigInteger.digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".split("");

/*
	Method: toString
	Convert a <BigInteger> to a string.

	When *base* is greater than 10, letters are upper case.

	Parameters:

		base - Optional base to represent the number in (default is base 10).
		       Must be between 2 and 36 inclusive, or an Error will be thrown.

	Returns:

		The string representation of the <BigInteger>.
*/
BigInteger.prototype.toString = function(base) {
	base = +base || 10;
	if (base < 2 || base > 36) {
		throw new Error("illegal radix " + base + ".");
	}
	if (this._s === 0) {
		return "0";
	}
	if (base === 10) {
		var str = this._s < 0 ? "-" : "";
		str += this._d[this._d.length - 1].toString();
		for (var i = this._d.length - 2; i >= 0; i--) {
			var group = this._d[i].toString();
			while (group.length < BigInteger_base_log10) group = '0' + group;
			str += group;
		}
		return str;
	}
	else {
		var numerals = BigInteger.digits;
		base = BigInteger.small[base];
		var sign = this._s;

		var n = this.abs();
		var digits = [];
		var digit;

		while (n._s !== 0) {
			var divmod = n.divRem(base);
			n = divmod[0];
			digit = divmod[1];
			// TODO: This could be changed to unshift instead of reversing at the end.
			// Benchmark both to compare speeds.
			digits.push(numerals[digit.valueOf()]);
		}
		return (sign < 0 ? "-" : "") + digits.reverse().join("");
	}
};

// Verify strings for parsing
BigInteger.radixRegex = [
	/^$/,
	/^$/,
	/^[01]*$/,
	/^[012]*$/,
	/^[0-3]*$/,
	/^[0-4]*$/,
	/^[0-5]*$/,
	/^[0-6]*$/,
	/^[0-7]*$/,
	/^[0-8]*$/,
	/^[0-9]*$/,
	/^[0-9aA]*$/,
	/^[0-9abAB]*$/,
	/^[0-9abcABC]*$/,
	/^[0-9a-dA-D]*$/,
	/^[0-9a-eA-E]*$/,
	/^[0-9a-fA-F]*$/,
	/^[0-9a-gA-G]*$/,
	/^[0-9a-hA-H]*$/,
	/^[0-9a-iA-I]*$/,
	/^[0-9a-jA-J]*$/,
	/^[0-9a-kA-K]*$/,
	/^[0-9a-lA-L]*$/,
	/^[0-9a-mA-M]*$/,
	/^[0-9a-nA-N]*$/,
	/^[0-9a-oA-O]*$/,
	/^[0-9a-pA-P]*$/,
	/^[0-9a-qA-Q]*$/,
	/^[0-9a-rA-R]*$/,
	/^[0-9a-sA-S]*$/,
	/^[0-9a-tA-T]*$/,
	/^[0-9a-uA-U]*$/,
	/^[0-9a-vA-V]*$/,
	/^[0-9a-wA-W]*$/,
	/^[0-9a-xA-X]*$/,
	/^[0-9a-yA-Y]*$/,
	/^[0-9a-zA-Z]*$/
];

/*
	Function: parse
	Parse a string into a <BigInteger>.

	*base* is optional but, if provided, must be from 2 to 36 inclusive. If
	*base* is not provided, it will be guessed based on the leading characters
	of *s* as follows:

	- "0x" or "0X": *base* = 16
	- "0c" or "0C": *base* = 8
	- "0b" or "0B": *base* = 2
	- else: *base* = 10

	If no base is provided, or *base* is 10, the number can be in exponential
	form. For example, these are all valid:

	> BigInteger.parse("1e9");              // Same as "1000000000"
	> BigInteger.parse("1.234*10^3");       // Same as 1234
	> BigInteger.parse("56789 * 10 ** -2"); // Same as 567

	If any characters fall outside the range defined by the radix, an exception
	will be thrown.

	Parameters:

		s - The string to parse.
		base - Optional radix (default is to guess based on *s*).

	Returns:

		a <BigInteger> instance.
*/
BigInteger.parse = function(s, base) {
	// Expands a number in exponential form to decimal form.
	// expandExponential("-13.441*10^5") === "1344100";
	// expandExponential("1.12300e-1") === "0.112300";
	// expandExponential(1000000000000000000000000000000) === "1000000000000000000000000000000";
	function expandExponential(str) {
		str = str.replace(/\s*[*xX]\s*10\s*(\^|\*\*)\s*/, "e");

		return str.replace(/^([+\-])?(\d+)\.?(\d*)[eE]([+\-]?\d+)$/, function(x, s, n, f, c) {
			c = +c;
			var l = c < 0;
			var i = n.length + c;
			x = (l ? n : f).length;
			c = ((c = Math.abs(c)) >= x ? c - x + l : 0);
			var z = (new Array(c + 1)).join("0");
			var r = n + f;
			return (s || "") + (l ? r = z + r : r += z).substr(0, i += l ? z.length : 0) + (i < r.length ? "." + r.substr(i) : "");
		});
	}

	s = s.toString();
	if (typeof base === "undefined" || +base === 10) {
		s = expandExponential(s);
	}

	var prefixRE;
	if (typeof base === "undefined") {
		prefixRE = '0[xcb]';
	}
	else if (base == 16) {
		prefixRE = '0x';
	}
	else if (base == 8) {
		prefixRE = '0c';
	}
	else if (base == 2) {
		prefixRE = '0b';
	}
	else {
		prefixRE = '';
	}
	var parts = new RegExp('^([+\\-]?)(' + prefixRE + ')?([0-9a-z]*)(?:\\.\\d*)?$', 'i').exec(s);
	if (parts) {
		var sign = parts[1] || "+";
		var baseSection = parts[2] || "";
		var digits = parts[3] || "";

		if (typeof base === "undefined") {
			// Guess base
			if (baseSection === "0x" || baseSection === "0X") { // Hex
				base = 16;
			}
			else if (baseSection === "0c" || baseSection === "0C") { // Octal
				base = 8;
			}
			else if (baseSection === "0b" || baseSection === "0B") { // Binary
				base = 2;
			}
			else {
				base = 10;
			}
		}
		else if (base < 2 || base > 36) {
			throw new Error("Illegal radix " + base + ".");
		}

		base = +base;

		// Check for digits outside the range
		if (!(BigInteger.radixRegex[base].test(digits))) {
			throw new Error("Bad digit for radix " + base);
		}

		// Strip leading zeros, and convert to array
		digits = digits.replace(/^0+/, "").split("");
		if (digits.length === 0) {
			return ZERO;
		}

		// Get the sign (we know it's not zero)
		sign = (sign === "-") ? -1 : 1;

		// Optimize 10
		if (base == 10) {
			var d = [];
			while (digits.length >= BigInteger_base_log10) {
				d.push(parseInt(digits.splice(digits.length-BigInteger.base_log10, BigInteger.base_log10).join(''), 10));
			}
			d.push(parseInt(digits.join(''), 10));
			return new BigInteger(d, sign, CONSTRUCT);
		}

		// Do the conversion
		var d = ZERO;
		base = BigInteger.small[base];
		var small = BigInteger.small;
		for (var i = 0; i < digits.length; i++) {
			d = d.multiply(base).add(small[parseInt(digits[i], 36)]);
		}
		return new BigInteger(d._d, sign, CONSTRUCT);
	}
	else {
		throw new Error("Invalid BigInteger format: " + s);
	}
};

/*
	Function: add
	Add two <BigIntegers>.

	Parameters:

		n - The number to add to *this*. Will be converted to a <BigInteger>.

	Returns:

		The numbers added together.

	See Also:

		<subtract>, <multiply>, <quotient>, <next>
*/
BigInteger.prototype.add = function(n) {
	if (this._s === 0) {
		return BigInteger(n);
	}

	n = BigInteger(n);
	if (n._s === 0) {
		return this;
	}
	if (this._s !== n._s) {
		n = n.negate();
		return this.subtract(n);
	}

	var a = this._d;
	var b = n._d;
	var al = a.length;
	var bl = b.length;
	var sum = new Array(Math.max(al, bl) + 1);
	var size = Math.min(al, bl);
	var carry = 0;
	var digit;

	for (var i = 0; i < size; i++) {
		digit = a[i] + b[i] + carry;
		sum[i] = digit % BigInteger_base;
		carry = (digit / BigInteger_base) | 0;
	}
	if (bl > al) {
		a = b;
		al = bl;
	}
	for (i = size; carry && i < al; i++) {
		digit = a[i] + carry;
		sum[i] = digit % BigInteger_base;
		carry = (digit / BigInteger_base) | 0;
	}
	if (carry) {
		sum[i] = carry;
	}

	for ( ; i < al; i++) {
		sum[i] = a[i];
	}

	return new BigInteger(sum, this._s, CONSTRUCT);
};

/*
	Function: negate
	Get the additive inverse of a <BigInteger>.

	Returns:

		A <BigInteger> with the same magnatude, but with the opposite sign.

	See Also:

		<abs>
*/
BigInteger.prototype.negate = function() {
	return new BigInteger(this._d, (-this._s) | 0, CONSTRUCT);
};

/*
	Function: abs
	Get the absolute value of a <BigInteger>.

	Returns:

		A <BigInteger> with the same magnatude, but always positive (or zero).

	See Also:

		<negate>
*/
BigInteger.prototype.abs = function() {
	return (this._s < 0) ? this.negate() : this;
};

/*
	Function: subtract
	Subtract two <BigIntegers>.

	Parameters:

		n - The number to subtract from *this*. Will be converted to a <BigInteger>.

	Returns:

		The *n* subtracted from *this*.

	See Also:

		<add>, <multiply>, <quotient>, <prev>
*/
BigInteger.prototype.subtract = function(n) {
	if (this._s === 0) {
		return BigInteger(n).negate();
	}

	n = BigInteger(n);
	if (n._s === 0) {
		return this;
	}
	if (this._s !== n._s) {
		n = n.negate();
		return this.add(n);
	}

	var m = this;
	// negative - negative => -|a| - -|b| => -|a| + |b| => |b| - |a|
	if (this._s < 0) {
		m = new BigInteger(n._d, 1, CONSTRUCT);
		n = new BigInteger(this._d, 1, CONSTRUCT);
	}

	// Both are positive => a - b
	var sign = m.compareAbs(n);
	if (sign === 0) {
		return ZERO;
	}
	else if (sign < 0) {
		// swap m and n
		var t = n;
		n = m;
		m = t;
	}

	// a > b
	var a = m._d;
	var b = n._d;
	var al = a.length;
	var bl = b.length;
	var diff = new Array(al); // al >= bl since a > b
	var borrow = 0;
	var i;
	var digit;

	for (i = 0; i < bl; i++) {
		digit = a[i] - borrow - b[i];
		if (digit < 0) {
			digit += BigInteger_base;
			borrow = 1;
		}
		else {
			borrow = 0;
		}
		diff[i] = digit;
	}
	for (i = bl; i < al; i++) {
		digit = a[i] - borrow;
		if (digit < 0) {
			digit += BigInteger_base;
		}
		else {
			diff[i++] = digit;
			break;
		}
		diff[i] = digit;
	}
	for ( ; i < al; i++) {
		diff[i] = a[i];
	}

	return new BigInteger(diff, sign, CONSTRUCT);
};

(function() {
	function addOne(n, sign) {
		var a = n._d;
		var sum = a.slice();
		var carry = true;
		var i = 0;

		while (true) {
			var digit = (a[i] || 0) + 1;
			sum[i] = digit % BigInteger_base;
			if (digit <= BigInteger_base - 1) {
				break;
			}
			++i;
		}

		return new BigInteger(sum, sign, CONSTRUCT);
	}

	function subtractOne(n, sign) {
		var a = n._d;
		var sum = a.slice();
		var borrow = true;
		var i = 0;

		while (true) {
			var digit = (a[i] || 0) - 1;
			if (digit < 0) {
				sum[i] = digit + BigInteger_base;
			}
			else {
				sum[i] = digit;
				break;
			}
			++i;
		}

		return new BigInteger(sum, sign, CONSTRUCT);
	}

	/*
		Function: next
		Get the next <BigInteger> (add one).

		Returns:

			*this* + 1.

		See Also:

			<add>, <prev>
	*/
	BigInteger.prototype.next = function() {
		switch (this._s) {
		case 0:
			return ONE;
		case -1:
			return subtractOne(this, -1);
		// case 1:
		default:
			return addOne(this, 1);
		}
	};

	/*
		Function: prev
		Get the previous <BigInteger> (subtract one).

		Returns:

			*this* - 1.

		See Also:

			<next>, <subtract>
	*/
	BigInteger.prototype.prev = function() {
		switch (this._s) {
		case 0:
			return M_ONE;
		case -1:
			return addOne(this, -1);
		// case 1:
		default:
			return subtractOne(this, 1);
		}
	};
})();

/*
	Function: compareAbs
	Compare the absolute value of two <BigIntegers>.

	Calling <compareAbs> is faster than calling <abs> twice, then <compare>.

	Parameters:

		n - The number to compare to *this*. Will be converted to a <BigInteger>.

	Returns:

		-1, 0, or +1 if *|this|* is less than, equal to, or greater than *|n|*.

	See Also:

		<compare>, <abs>
*/
BigInteger.prototype.compareAbs = function(n) {
	if (this === n) {
		return 0;
	}

	if (!(n instanceof BigInteger)) {
		if (!isFinite(n)) {
			return(isNaN(n) ? n : -1);
		}
		n = BigInteger(n);
	}

	if (this._s === 0) {
		return (n._s !== 0) ? -1 : 0;
	}
	if (n._s === 0) {
		return 1;
	}

	var l = this._d.length;
	var nl = n._d.length;
	if (l < nl) {
		return -1;
	}
	else if (l > nl) {
		return 1;
	}

	var a = this._d;
	var b = n._d;
	for (var i = l-1; i >= 0; i--) {
		if (a[i] !== b[i]) {
			return a[i] < b[i] ? -1 : 1;
		}
	}

	return 0;
};

/*
	Function: compare
	Compare two <BigIntegers>.

	Parameters:

		n - The number to compare to *this*. Will be converted to a <BigInteger>.

	Returns:

		-1, 0, or +1 if *this* is less than, equal to, or greater than *n*.

	See Also:

		<compareAbs>, <isPositive>, <isNegative>, <isUnit>
*/
BigInteger.prototype.compare = function(n) {
	if (this === n) {
		return 0;
	}

	n = BigInteger(n);

	if (this._s === 0) {
		return -n._s;
	}

	if (this._s === n._s) { // both positive or both negative
		var cmp = this.compareAbs(n);
		return cmp * this._s;
	}
	else {
		return this._s;
	}
};

/*
	Function: isUnit
	Return true iff *this* is either 1 or -1.

	Returns:

		true if *this* compares equal to <BigInteger.ONE> or <BigInteger.M_ONE>.

	See Also:

		<isZero>, <isNegative>, <isPositive>, <compareAbs>, <compare>,
		<BigInteger.ONE>, <BigInteger.M_ONE>
*/
BigInteger.prototype.isUnit = function() {
	return this === ONE ||
		this === M_ONE ||
		(this._d.length === 1 && this._d[0] === 1);
};

/*
	Function: multiply
	Multiply two <BigIntegers>.

	Parameters:

		n - The number to multiply *this* by. Will be converted to a
		<BigInteger>.

	Returns:

		The numbers multiplied together.

	See Also:

		<add>, <subtract>, <quotient>, <square>
*/
BigInteger.prototype.multiply = function(n) {
	// TODO: Consider adding Karatsuba multiplication for large numbers
	if (this._s === 0) {
		return ZERO;
	}

	n = BigInteger(n);
	if (n._s === 0) {
		return ZERO;
	}
	if (this.isUnit()) {
		if (this._s < 0) {
			return n.negate();
		}
		return n;
	}
	if (n.isUnit()) {
		if (n._s < 0) {
			return this.negate();
		}
		return this;
	}
	if (this === n) {
		return this.square();
	}

	var r = (this._d.length >= n._d.length);
	var a = (r ? this : n)._d; // a will be longer than b
	var b = (r ? n : this)._d;
	var al = a.length;
	var bl = b.length;

	var pl = al + bl;
	var partial = new Array(pl);
	var i;
	for (i = 0; i < pl; i++) {
		partial[i] = 0;
	}

	for (i = 0; i < bl; i++) {
		var carry = 0;
		var bi = b[i];
		var jlimit = al + i;
		var digit;
		for (var j = i; j < jlimit; j++) {
			digit = partial[j] + bi * a[j - i] + carry;
			carry = (digit / BigInteger_base) | 0;
			partial[j] = (digit % BigInteger_base) | 0;
		}
		if (carry) {
			digit = partial[j] + carry;
			carry = (digit / BigInteger_base) | 0;
			partial[j] = digit % BigInteger_base;
		}
	}
	return new BigInteger(partial, this._s * n._s, CONSTRUCT);
};

// Multiply a BigInteger by a single-digit native number
// Assumes that this and n are >= 0
// This is not really intended to be used outside the library itself
BigInteger.prototype.multiplySingleDigit = function(n) {
	if (n === 0 || this._s === 0) {
		return ZERO;
	}
	if (n === 1) {
		return this;
	}

	var digit;
	if (this._d.length === 1) {
		digit = this._d[0] * n;
		if (digit >= BigInteger_base) {
			return new BigInteger([(digit % BigInteger_base)|0,
					(digit / BigInteger_base)|0], 1, CONSTRUCT);
		}
		return new BigInteger([digit], 1, CONSTRUCT);
	}

	if (n === 2) {
		return this.add(this);
	}
	if (this.isUnit()) {
		return new BigInteger([n], 1, CONSTRUCT);
	}

	var a = this._d;
	var al = a.length;

	var pl = al + 1;
	var partial = new Array(pl);
	for (var i = 0; i < pl; i++) {
		partial[i] = 0;
	}

	var carry = 0;
	for (var j = 0; j < al; j++) {
		digit = n * a[j] + carry;
		carry = (digit / BigInteger_base) | 0;
		partial[j] = (digit % BigInteger_base) | 0;
	}
	if (carry) {
		partial[j] = carry;
	}

	return new BigInteger(partial, 1, CONSTRUCT);
};

/*
	Function: square
	Multiply a <BigInteger> by itself.

	This is slightly faster than regular multiplication, since it removes the
	duplicated multiplcations.

	Returns:

		> this.multiply(this)

	See Also:
		<multiply>
*/
BigInteger.prototype.square = function() {
	// Normally, squaring a 10-digit number would take 100 multiplications.
	// Of these 10 are unique diagonals, of the remaining 90 (100-10), 45 are repeated.
	// This procedure saves (N*(N-1))/2 multiplications, (e.g., 45 of 100 multiplies).
	// Based on code by Gary Darby, Intellitech Systems Inc., www.DelphiForFun.org

	if (this._s === 0) {
		return ZERO;
	}
	if (this.isUnit()) {
		return ONE;
	}

	var digits = this._d;
	var length = digits.length;
	var imult1 = new Array(length + length + 1);
	var product, carry, k;
	var i;

	// Calculate diagonal
	for (i = 0; i < length; i++) {
		k = i * 2;
		product = digits[i] * digits[i];
		carry = (product / BigInteger_base) | 0;
		imult1[k] = product % BigInteger_base;
		imult1[k + 1] = carry;
	}

	// Calculate repeating part
	for (i = 0; i < length; i++) {
		carry = 0;
		k = i * 2 + 1;
		for (var j = i + 1; j < length; j++, k++) {
			product = digits[j] * digits[i] * 2 + imult1[k] + carry;
			carry = (product / BigInteger_base) | 0;
			imult1[k] = product % BigInteger_base;
		}
		k = length + i;
		var digit = carry + imult1[k];
		carry = (digit / BigInteger_base) | 0;
		imult1[k] = digit % BigInteger_base;
		imult1[k + 1] += carry;
	}

	return new BigInteger(imult1, 1, CONSTRUCT);
};

/*
	Function: quotient
	Divide two <BigIntegers> and truncate towards zero.

	<quotient> throws an exception if *n* is zero.

	Parameters:

		n - The number to divide *this* by. Will be converted to a <BigInteger>.

	Returns:

		The *this* / *n*, truncated to an integer.

	See Also:

		<add>, <subtract>, <multiply>, <divRem>, <remainder>
*/
BigInteger.prototype.quotient = function(n) {
	return this.divRem(n)[0];
};

/*
	Function: divide
	Deprecated synonym for <quotient>.
*/
BigInteger.prototype.divide = BigInteger.prototype.quotient;

/*
	Function: remainder
	Calculate the remainder of two <BigIntegers>.

	<remainder> throws an exception if *n* is zero.

	Parameters:

		n - The remainder after *this* is divided *this* by *n*. Will be
		    converted to a <BigInteger>.

	Returns:

		*this* % *n*.

	See Also:

		<divRem>, <quotient>
*/
BigInteger.prototype.remainder = function(n) {
	return this.divRem(n)[1];
};

/*
	Function: divRem
	Calculate the integer quotient and remainder of two <BigIntegers>.

	<divRem> throws an exception if *n* is zero.

	Parameters:

		n - The number to divide *this* by. Will be converted to a <BigInteger>.

	Returns:

		A two-element array containing the quotient and the remainder.

		> a.divRem(b)

		is exactly equivalent to

		> [a.quotient(b), a.remainder(b)]

		except it is faster, because they are calculated at the same time.

	See Also:

		<quotient>, <remainder>
*/
BigInteger.prototype.divRem = function(n) {
	n = BigInteger(n);
	if (n._s === 0) {
		throw new Error("Divide by zero");
	}
	if (this._s === 0) {
		return [ZERO, ZERO];
	}
	if (n._d.length === 1) {
		return this.divRemSmall(n._s * n._d[0]);
	}

	// Test for easy cases -- |n1| <= |n2|
	switch (this.compareAbs(n)) {
	case 0: // n1 == n2
		return [this._s === n._s ? ONE : M_ONE, ZERO];
	case -1: // |n1| < |n2|
		return [ZERO, this];
	}

	var sign = this._s * n._s;
	var a = n.abs();
	var b_digits = this._d;
	var b_index = b_digits.length;
	var digits = n._d.length;
	var quot = [];
	var guess;

	var part = new BigInteger([], 0, CONSTRUCT);
	part._s = 1;

	while (b_index) {
		part._d.unshift(b_digits[--b_index]);

		if (part.compareAbs(n) < 0) {
			quot.push(0);
			continue;
		}
		if (part._s === 0) {
			guess = 0;
		}
		else {
			var xlen = part._d.length, ylen = a._d.length;
			var highx = part._d[xlen-1]*BigInteger_base + part._d[xlen-2];
			var highy = a._d[ylen-1]*BigInteger_base + a._d[ylen-2];
			if (part._d.length > a._d.length) {
				// The length of part._d can either match a._d length,
				// or exceed it by one.
				highx = (highx+1)*BigInteger_base;
			}
			guess = Math.ceil(highx/highy);
		}
		do {
			var check = a.multiplySingleDigit(guess);
			if (check.compareAbs(part) <= 0) {
				break;
			}
			guess--;
		} while (guess);

		quot.push(guess);
		if (!guess) {
			continue;
		}
		var diff = part.subtract(check);
		part._d = diff._d.slice();
		if (part._d.length === 0) {
			part._s = 0;
		}
	}

	return [new BigInteger(quot.reverse(), sign, CONSTRUCT),
		   new BigInteger(part._d, this._s, CONSTRUCT)];
};

// Throws an exception if n is outside of (-BigInteger.base, -1] or
// [1, BigInteger.base).  It's not necessary to call this, since the
// other division functions will call it if they are able to.
BigInteger.prototype.divRemSmall = function(n) {
	var r;
	n = +n;
	if (n === 0) {
		throw new Error("Divide by zero");
	}

	var n_s = n < 0 ? -1 : 1;
	var sign = this._s * n_s;
	n = Math.abs(n);

	if (n < 1 || n >= BigInteger_base) {
		throw new Error("Argument out of range");
	}

	if (this._s === 0) {
		return [ZERO, ZERO];
	}

	if (n === 1 || n === -1) {
		return [(sign === 1) ? this.abs() : new BigInteger(this._d, sign, CONSTRUCT), ZERO];
	}

	// 2 <= n < BigInteger_base

	// divide a single digit by a single digit
	if (this._d.length === 1) {
		var q = new BigInteger([(this._d[0] / n) | 0], 1, CONSTRUCT);
		r = new BigInteger([(this._d[0] % n) | 0], 1, CONSTRUCT);
		if (sign < 0) {
			q = q.negate();
		}
		if (this._s < 0) {
			r = r.negate();
		}
		return [q, r];
	}

	var digits = this._d.slice();
	var quot = new Array(digits.length);
	var part = 0;
	var diff = 0;
	var i = 0;
	var guess;

	while (digits.length) {
		part = part * BigInteger_base + digits[digits.length - 1];
		if (part < n) {
			quot[i++] = 0;
			digits.pop();
			diff = BigInteger_base * diff + part;
			continue;
		}
		if (part === 0) {
			guess = 0;
		}
		else {
			guess = (part / n) | 0;
		}

		var check = n * guess;
		diff = part - check;
		quot[i++] = guess;
		if (!guess) {
			digits.pop();
			continue;
		}

		digits.pop();
		part = diff;
	}

	r = new BigInteger([diff], 1, CONSTRUCT);
	if (this._s < 0) {
		r = r.negate();
	}
	return [new BigInteger(quot.reverse(), sign, CONSTRUCT), r];
};

/*
	Function: isEven
	Return true iff *this* is divisible by two.

	Note that <BigInteger.ZERO> is even.

	Returns:

		true if *this* is even, false otherwise.

	See Also:

		<isOdd>
*/
BigInteger.prototype.isEven = function() {
	var digits = this._d;
	return this._s === 0 || digits.length === 0 || (digits[0] % 2) === 0;
};

/*
	Function: isOdd
	Return true iff *this* is not divisible by two.

	Returns:

		true if *this* is odd, false otherwise.

	See Also:

		<isEven>
*/
BigInteger.prototype.isOdd = function() {
	return !this.isEven();
};

/*
	Function: sign
	Get the sign of a <BigInteger>.

	Returns:

		* -1 if *this* < 0
		* 0 if *this* == 0
		* +1 if *this* > 0

	See Also:

		<isZero>, <isPositive>, <isNegative>, <compare>, <BigInteger.ZERO>
*/
BigInteger.prototype.sign = function() {
	return this._s;
};

/*
	Function: isPositive
	Return true iff *this* > 0.

	Returns:

		true if *this*.compare(<BigInteger.ZERO>) == 1.

	See Also:

		<sign>, <isZero>, <isNegative>, <isUnit>, <compare>, <BigInteger.ZERO>
*/
BigInteger.prototype.isPositive = function() {
	return this._s > 0;
};

/*
	Function: isNegative
	Return true iff *this* < 0.

	Returns:

		true if *this*.compare(<BigInteger.ZERO>) == -1.

	See Also:

		<sign>, <isPositive>, <isZero>, <isUnit>, <compare>, <BigInteger.ZERO>
*/
BigInteger.prototype.isNegative = function() {
	return this._s < 0;
};

/*
	Function: isZero
	Return true iff *this* == 0.

	Returns:

		true if *this*.compare(<BigInteger.ZERO>) == 0.

	See Also:

		<sign>, <isPositive>, <isNegative>, <isUnit>, <BigInteger.ZERO>
*/
BigInteger.prototype.isZero = function() {
	return this._s === 0;
};

/*
	Function: exp10
	Multiply a <BigInteger> by a power of 10.

	This is equivalent to, but faster than

	> if (n >= 0) {
	>     return this.multiply(BigInteger("1e" + n));
	> }
	> else { // n <= 0
	>     return this.quotient(BigInteger("1e" + -n));
	> }

	Parameters:

		n - The power of 10 to multiply *this* by. *n* is converted to a
		javascipt number and must be no greater than <BigInteger.MAX_EXP>
		(0x7FFFFFFF), or an exception will be thrown.

	Returns:

		*this* * (10 ** *n*), truncated to an integer if necessary.

	See Also:

		<pow>, <multiply>
*/
BigInteger.prototype.exp10 = function(n) {
	n = +n;
	if (n === 0) {
		return this;
	}
	if (Math.abs(n) > Number(MAX_EXP)) {
		throw new Error("exponent too large in BigInteger.exp10");
	}
	if (n > 0) {
		var k = new BigInteger(this._d.slice(), this._s, CONSTRUCT);

		for (; n >= BigInteger_base_log10; n -= BigInteger_base_log10) {
			k._d.unshift(0);
		}
		if (n == 0)
			return k;
		k._s = 1;
		k = k.multiplySingleDigit(Math.pow(10, n));
		return (this._s < 0 ? k.negate() : k);
	} else if (-n >= this._d.length*BigInteger_base_log10) {
		return ZERO;
	} else {
		var k = new BigInteger(this._d.slice(), this._s, CONSTRUCT);

		for (n = -n; n >= BigInteger_base_log10; n -= BigInteger_base_log10) {
			k._d.shift();
		}
		return (n == 0) ? k : k.divRemSmall(Math.pow(10, n))[0];
	}
};

/*
	Function: pow
	Raise a <BigInteger> to a power.

	In this implementation, 0**0 is 1.

	Parameters:

		n - The exponent to raise *this* by. *n* must be no greater than
		<BigInteger.MAX_EXP> (0x7FFFFFFF), or an exception will be thrown.

	Returns:

		*this* raised to the *nth* power.

	See Also:

		<modPow>
*/
BigInteger.prototype.pow = function(n) {
	if (this.isUnit()) {
		if (this._s > 0) {
			return this;
		}
		else {
			return BigInteger(n).isOdd() ? this : this.negate();
		}
	}

	n = BigInteger(n);
	if (n._s === 0) {
		return ONE;
	}
	else if (n._s < 0) {
		if (this._s === 0) {
			throw new Error("Divide by zero");
		}
		else {
			return ZERO;
		}
	}
	if (this._s === 0) {
		return ZERO;
	}
	if (n.isUnit()) {
		return this;
	}

	if (n.compareAbs(MAX_EXP) > 0) {
		throw new Error("exponent too large in BigInteger.pow");
	}
	var x = this;
	var aux = ONE;
	var two = BigInteger.small[2];

	while (n.isPositive()) {
		if (n.isOdd()) {
			aux = aux.multiply(x);
			if (n.isUnit()) {
				return aux;
			}
		}
		x = x.square();
		n = n.quotient(two);
	}

	return aux;
};

/*
	Function: modPow
	Raise a <BigInteger> to a power (mod m).

	Because it is reduced by a modulus, <modPow> is not limited by
	<BigInteger.MAX_EXP> like <pow>.

	Parameters:

		exponent - The exponent to raise *this* by. Must be positive.
		modulus - The modulus.

	Returns:

		*this* ^ *exponent* (mod *modulus*).

	See Also:

		<pow>, <mod>
*/
BigInteger.prototype.modPow = function(exponent, modulus) {
	var result = ONE;
	var base = this;

	while (exponent.isPositive()) {
		if (exponent.isOdd()) {
			result = result.multiply(base).remainder(modulus);
		}

		exponent = exponent.quotient(BigInteger.small[2]);
		if (exponent.isPositive()) {
			base = base.square().remainder(modulus);
		}
	}

	return result;
};

/*
	Function: log
	Get the natural logarithm of a <BigInteger> as a native JavaScript number.

	This is equivalent to

	> Math.log(this.toJSValue())

	but handles values outside of the native number range.

	Returns:

		log( *this* )

	See Also:

		<toJSValue>
*/
BigInteger.prototype.log = function() {
	switch (this._s) {
	case 0:	 return -Infinity;
	case -1: return NaN;
	default: // Fall through.
	}

	var l = this._d.length;

	if (l*BigInteger_base_log10 < 30) {
		return Math.log(this.valueOf());
	}

	var N = Math.ceil(30/BigInteger_base_log10);
	var firstNdigits = this._d.slice(l - N);
	return Math.log((new BigInteger(firstNdigits, 1, CONSTRUCT)).valueOf()) + (l - N) * Math.log(BigInteger_base);
};

/*
	Function: valueOf
	Convert a <BigInteger> to a native JavaScript integer.

	This is called automatically by JavaScipt to convert a <BigInteger> to a
	native value.

	Returns:

		> parseInt(this.toString(), 10)

	See Also:

		<toString>, <toJSValue>
*/
BigInteger.prototype.valueOf = function() {
	return parseInt(this.toString(), 10);
};

/*
	Function: toJSValue
	Convert a <BigInteger> to a native JavaScript integer.

	This is the same as valueOf, but more explicitly named.

	Returns:

		> parseInt(this.toString(), 10)

	See Also:

		<toString>, <valueOf>
*/
BigInteger.prototype.toJSValue = function() {
	return parseInt(this.toString(), 10);
};

var MAX_EXP = BigInteger(0x7FFFFFFF);
// Constant: MAX_EXP
// The largest exponent allowed in <pow> and <exp10> (0x7FFFFFFF or 2147483647).
BigInteger.MAX_EXP = MAX_EXP;

(function() {
	function makeUnary(fn) {
		return function(a) {
			return fn.call(BigInteger(a));
		};
	}

	function makeBinary(fn) {
		return function(a, b) {
			return fn.call(BigInteger(a), BigInteger(b));
		};
	}

	function makeTrinary(fn) {
		return function(a, b, c) {
			return fn.call(BigInteger(a), BigInteger(b), BigInteger(c));
		};
	}

	(function() {
		var i, fn;
		var unary = "toJSValue,isEven,isOdd,sign,isZero,isNegative,abs,isUnit,square,negate,isPositive,toString,next,prev,log".split(",");
		var binary = "compare,remainder,divRem,subtract,add,quotient,divide,multiply,pow,compareAbs".split(",");
		var trinary = ["modPow"];

		for (i = 0; i < unary.length; i++) {
			fn = unary[i];
			BigInteger[fn] = makeUnary(BigInteger.prototype[fn]);
		}

		for (i = 0; i < binary.length; i++) {
			fn = binary[i];
			BigInteger[fn] = makeBinary(BigInteger.prototype[fn]);
		}

		for (i = 0; i < trinary.length; i++) {
			fn = trinary[i];
			BigInteger[fn] = makeTrinary(BigInteger.prototype[fn]);
		}

		BigInteger.exp10 = function(x, n) {
			return BigInteger(x).exp10(n);
		};
	})();
})();

exports.BigInteger = BigInteger;
})(typeof exports !== 'undefined' ? exports : this);

/*
 * Configurable variables. You may need to tweak these to be compatible with
 * the server-side, but the defaults work in most cases.
 */
var hexcase = 0;   /* hex output format. 0 - lowercase; 1 - uppercase        */
var b64pad  = "";  /* base-64 pad character. "=" for strict RFC compliance   */

/*
 * These are the functions you'll usually want to call
 * They take string arguments and return either hex or base-64 encoded strings
 */
function hex_md5(s)    { return rstr2hex(rstr_md5(str2rstr_utf8(s))); }
function b64_md5(s)    { return rstr2b64(rstr_md5(str2rstr_utf8(s))); }
function any_md5(s, e) { return rstr2any(rstr_md5(str2rstr_utf8(s)), e); }
function hex_hmac_md5(k, d)
  { return rstr2hex(rstr_hmac_md5(str2rstr_utf8(k), str2rstr_utf8(d))); }
function b64_hmac_md5(k, d)
  { return rstr2b64(rstr_hmac_md5(str2rstr_utf8(k), str2rstr_utf8(d))); }
function any_hmac_md5(k, d, e)
  { return rstr2any(rstr_hmac_md5(str2rstr_utf8(k), str2rstr_utf8(d)), e); }

/*
 * Perform a simple self-test to see if the VM is working
 */
function md5_vm_test()
{
  return hex_md5("abc").toLowerCase() == "900150983cd24fb0d6963f7d28e17f72";
}

/*
 * Calculate the MD5 of a raw string
 */
function rstr_md5(s)
{
  return binl2rstr(binl_md5(rstr2binl(s), s.length * 8));
}

/*
 * Calculate the HMAC-MD5, of a key and some data (raw strings)
 */
function rstr_hmac_md5(key, data)
{
  var bkey = rstr2binl(key);
  if(bkey.length > 16) bkey = binl_md5(bkey, key.length * 8);

  var ipad = Array(16), opad = Array(16);
  for(var i = 0; i < 16; i++)
  {
    ipad[i] = bkey[i] ^ 0x36363636;
    opad[i] = bkey[i] ^ 0x5C5C5C5C;
  }

  var hash = binl_md5(ipad.concat(rstr2binl(data)), 512 + data.length * 8);
  return binl2rstr(binl_md5(opad.concat(hash), 512 + 128));
}

/*
 * Convert a raw string to a hex string
 */
function rstr2hex(input)
{
  try { hexcase } catch(e) { hexcase=0; }
  var hex_tab = hexcase ? "0123456789ABCDEF" : "0123456789abcdef";
  var output = "";
  var x;
  for(var i = 0; i < input.length; i++)
  {
    x = input.charCodeAt(i);
    output += hex_tab.charAt((x >>> 4) & 0x0F)
           +  hex_tab.charAt( x        & 0x0F);
  }
  return output;
}

/*
 * Convert a raw string to a base-64 string
 */
function rstr2b64(input)
{
  try { b64pad } catch(e) { b64pad=''; }
  var tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  var output = "";
  var len = input.length;
  for(var i = 0; i < len; i += 3)
  {
    var triplet = (input.charCodeAt(i) << 16)
                | (i + 1 < len ? input.charCodeAt(i+1) << 8 : 0)
                | (i + 2 < len ? input.charCodeAt(i+2)      : 0);
    for(var j = 0; j < 4; j++)
    {
      if(i * 8 + j * 6 > input.length * 8) output += b64pad;
      else output += tab.charAt((triplet >>> 6*(3-j)) & 0x3F);
    }
  }
  return output;
}

/*
 * Convert a raw string to an arbitrary string encoding
 */
function rstr2any(input, encoding)
{
  var divisor = encoding.length;
  var i, j, q, x, quotient;

  /* Convert to an array of 16-bit big-endian values, forming the dividend */
  var dividend = Array(Math.ceil(input.length / 2));
  for(i = 0; i < dividend.length; i++)
  {
    dividend[i] = (input.charCodeAt(i * 2) << 8) | input.charCodeAt(i * 2 + 1);
  }

  /*
   * Repeatedly perform a long division. The binary array forms the dividend,
   * the length of the encoding is the divisor. Once computed, the quotient
   * forms the dividend for the next step. All remainders are stored for later
   * use.
   */
  var full_length = Math.ceil(input.length * 8 /
                                    (Math.log(encoding.length) / Math.log(2)));
  var remainders = Array(full_length);
  for(j = 0; j < full_length; j++)
  {
    quotient = Array();
    x = 0;
    for(i = 0; i < dividend.length; i++)
    {
      x = (x << 16) + dividend[i];
      q = Math.floor(x / divisor);
      x -= q * divisor;
      if(quotient.length > 0 || q > 0)
        quotient[quotient.length] = q;
    }
    remainders[j] = x;
    dividend = quotient;
  }

  /* Convert the remainders to the output string */
  var output = "";
  for(i = remainders.length - 1; i >= 0; i--)
    output += encoding.charAt(remainders[i]);

  return output;
}

/*
 * Encode a string as utf-8.
 * For efficiency, this assumes the input is valid utf-16.
 */
function str2rstr_utf8(input)
{
  var output = "";
  var i = -1;
  var x, y;

  while(++i < input.length)
  {
    /* Decode utf-16 surrogate pairs */
    x = input.charCodeAt(i);
    y = i + 1 < input.length ? input.charCodeAt(i + 1) : 0;
    if(0xD800 <= x && x <= 0xDBFF && 0xDC00 <= y && y <= 0xDFFF)
    {
      x = 0x10000 + ((x & 0x03FF) << 10) + (y & 0x03FF);
      i++;
    }

    /* Encode output as utf-8 */
    if(x <= 0x7F)
      output += String.fromCharCode(x);
    else if(x <= 0x7FF)
      output += String.fromCharCode(0xC0 | ((x >>> 6 ) & 0x1F),
                                    0x80 | ( x         & 0x3F));
    else if(x <= 0xFFFF)
      output += String.fromCharCode(0xE0 | ((x >>> 12) & 0x0F),
                                    0x80 | ((x >>> 6 ) & 0x3F),
                                    0x80 | ( x         & 0x3F));
    else if(x <= 0x1FFFFF)
      output += String.fromCharCode(0xF0 | ((x >>> 18) & 0x07),
                                    0x80 | ((x >>> 12) & 0x3F),
                                    0x80 | ((x >>> 6 ) & 0x3F),
                                    0x80 | ( x         & 0x3F));
  }
  return output;
}

/*
 * Encode a string as utf-16
 */
function str2rstr_utf16le(input)
{
  var output = "";
  for(var i = 0; i < input.length; i++)
    output += String.fromCharCode( input.charCodeAt(i)        & 0xFF,
                                  (input.charCodeAt(i) >>> 8) & 0xFF);
  return output;
}

function str2rstr_utf16be(input)
{
  var output = "";
  for(var i = 0; i < input.length; i++)
    output += String.fromCharCode((input.charCodeAt(i) >>> 8) & 0xFF,
                                   input.charCodeAt(i)        & 0xFF);
  return output;
}

/*
 * Convert a raw string to an array of little-endian words
 * Characters >255 have their high-byte silently ignored.
 */
function rstr2binl(input)
{
  var output = Array(input.length >> 2);
  for(var i = 0; i < output.length; i++)
    output[i] = 0;
  for(var i = 0; i < input.length * 8; i += 8)
    output[i>>5] |= (input.charCodeAt(i / 8) & 0xFF) << (i%32);
  return output;
}

/*
 * Convert an array of little-endian words to a string
 */
function binl2rstr(input)
{
  var output = "";
  for(var i = 0; i < input.length * 32; i += 8)
    output += String.fromCharCode((input[i>>5] >>> (i % 32)) & 0xFF);
  return output;
}

/*
 * Calculate the MD5 of an array of little-endian words, and a bit length.
 */
function binl_md5(x, len)
{
  /* append padding */
  x[len >> 5] |= 0x80 << ((len) % 32);
  x[(((len + 64) >>> 9) << 4) + 14] = len;

  var a =  1732584193;
  var b = -271733879;
  var c = -1732584194;
  var d =  271733878;

  for(var i = 0; i < x.length; i += 16)
  {
    var olda = a;
    var oldb = b;
    var oldc = c;
    var oldd = d;

    a = md5_ff(a, b, c, d, x[i+ 0], 7 , -680876936);
    d = md5_ff(d, a, b, c, x[i+ 1], 12, -389564586);
    c = md5_ff(c, d, a, b, x[i+ 2], 17,  606105819);
    b = md5_ff(b, c, d, a, x[i+ 3], 22, -1044525330);
    a = md5_ff(a, b, c, d, x[i+ 4], 7 , -176418897);
    d = md5_ff(d, a, b, c, x[i+ 5], 12,  1200080426);
    c = md5_ff(c, d, a, b, x[i+ 6], 17, -1473231341);
    b = md5_ff(b, c, d, a, x[i+ 7], 22, -45705983);
    a = md5_ff(a, b, c, d, x[i+ 8], 7 ,  1770035416);
    d = md5_ff(d, a, b, c, x[i+ 9], 12, -1958414417);
    c = md5_ff(c, d, a, b, x[i+10], 17, -42063);
    b = md5_ff(b, c, d, a, x[i+11], 22, -1990404162);
    a = md5_ff(a, b, c, d, x[i+12], 7 ,  1804603682);
    d = md5_ff(d, a, b, c, x[i+13], 12, -40341101);
    c = md5_ff(c, d, a, b, x[i+14], 17, -1502002290);
    b = md5_ff(b, c, d, a, x[i+15], 22,  1236535329);

    a = md5_gg(a, b, c, d, x[i+ 1], 5 , -165796510);
    d = md5_gg(d, a, b, c, x[i+ 6], 9 , -1069501632);
    c = md5_gg(c, d, a, b, x[i+11], 14,  643717713);
    b = md5_gg(b, c, d, a, x[i+ 0], 20, -373897302);
    a = md5_gg(a, b, c, d, x[i+ 5], 5 , -701558691);
    d = md5_gg(d, a, b, c, x[i+10], 9 ,  38016083);
    c = md5_gg(c, d, a, b, x[i+15], 14, -660478335);
    b = md5_gg(b, c, d, a, x[i+ 4], 20, -405537848);
    a = md5_gg(a, b, c, d, x[i+ 9], 5 ,  568446438);
    d = md5_gg(d, a, b, c, x[i+14], 9 , -1019803690);
    c = md5_gg(c, d, a, b, x[i+ 3], 14, -187363961);
    b = md5_gg(b, c, d, a, x[i+ 8], 20,  1163531501);
    a = md5_gg(a, b, c, d, x[i+13], 5 , -1444681467);
    d = md5_gg(d, a, b, c, x[i+ 2], 9 , -51403784);
    c = md5_gg(c, d, a, b, x[i+ 7], 14,  1735328473);
    b = md5_gg(b, c, d, a, x[i+12], 20, -1926607734);

    a = md5_hh(a, b, c, d, x[i+ 5], 4 , -378558);
    d = md5_hh(d, a, b, c, x[i+ 8], 11, -2022574463);
    c = md5_hh(c, d, a, b, x[i+11], 16,  1839030562);
    b = md5_hh(b, c, d, a, x[i+14], 23, -35309556);
    a = md5_hh(a, b, c, d, x[i+ 1], 4 , -1530992060);
    d = md5_hh(d, a, b, c, x[i+ 4], 11,  1272893353);
    c = md5_hh(c, d, a, b, x[i+ 7], 16, -155497632);
    b = md5_hh(b, c, d, a, x[i+10], 23, -1094730640);
    a = md5_hh(a, b, c, d, x[i+13], 4 ,  681279174);
    d = md5_hh(d, a, b, c, x[i+ 0], 11, -358537222);
    c = md5_hh(c, d, a, b, x[i+ 3], 16, -722521979);
    b = md5_hh(b, c, d, a, x[i+ 6], 23,  76029189);
    a = md5_hh(a, b, c, d, x[i+ 9], 4 , -640364487);
    d = md5_hh(d, a, b, c, x[i+12], 11, -421815835);
    c = md5_hh(c, d, a, b, x[i+15], 16,  530742520);
    b = md5_hh(b, c, d, a, x[i+ 2], 23, -995338651);

    a = md5_ii(a, b, c, d, x[i+ 0], 6 , -198630844);
    d = md5_ii(d, a, b, c, x[i+ 7], 10,  1126891415);
    c = md5_ii(c, d, a, b, x[i+14], 15, -1416354905);
    b = md5_ii(b, c, d, a, x[i+ 5], 21, -57434055);
    a = md5_ii(a, b, c, d, x[i+12], 6 ,  1700485571);
    d = md5_ii(d, a, b, c, x[i+ 3], 10, -1894986606);
    c = md5_ii(c, d, a, b, x[i+10], 15, -1051523);
    b = md5_ii(b, c, d, a, x[i+ 1], 21, -2054922799);
    a = md5_ii(a, b, c, d, x[i+ 8], 6 ,  1873313359);
    d = md5_ii(d, a, b, c, x[i+15], 10, -30611744);
    c = md5_ii(c, d, a, b, x[i+ 6], 15, -1560198380);
    b = md5_ii(b, c, d, a, x[i+13], 21,  1309151649);
    a = md5_ii(a, b, c, d, x[i+ 4], 6 , -145523070);
    d = md5_ii(d, a, b, c, x[i+11], 10, -1120210379);
    c = md5_ii(c, d, a, b, x[i+ 2], 15,  718787259);
    b = md5_ii(b, c, d, a, x[i+ 9], 21, -343485551);

    a = safe_add(a, olda);
    b = safe_add(b, oldb);
    c = safe_add(c, oldc);
    d = safe_add(d, oldd);
  }
  return Array(a, b, c, d);
}

/*
 * These functions implement the four basic operations the algorithm uses.
 */
function md5_cmn(q, a, b, x, s, t)
{
  return safe_add(bit_rol(safe_add(safe_add(a, q), safe_add(x, t)), s),b);
}
function md5_ff(a, b, c, d, x, s, t)
{
  return md5_cmn((b & c) | ((~b) & d), a, b, x, s, t);
}
function md5_gg(a, b, c, d, x, s, t)
{
  return md5_cmn((b & d) | (c & (~d)), a, b, x, s, t);
}
function md5_hh(a, b, c, d, x, s, t)
{
  return md5_cmn(b ^ c ^ d, a, b, x, s, t);
}
function md5_ii(a, b, c, d, x, s, t)
{
  return md5_cmn(c ^ (b | (~d)), a, b, x, s, t);
}

/*
 * Add integers, wrapping at 2^32. This uses 16-bit operations internally
 * to work around bugs in some JS interpreters.
 */
function safe_add(x, y)
{
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
}

/*
 * Bitwise rotate a 32-bit number to the left.
 */
function bit_rol(num, cnt)
{
  return (num << cnt) | (num >>> (32 - cnt));
}

  var BigInteger = exports.BigInteger;
  delete exports.BigInteger;

  var bcrypt_alphabet = './0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

  var valueSplit = function(bignum) {
    var self = {};
    self.take = function(N) {
      mod = bignum.remainder(N)
      bignum = bignum.divide(N)
      return parseInt(mod.toString(), 10);
    };
    self.choice = function(letters) {
      return letters[self.take(letters.length)];
    };
    return self;
  };

  var convertBase = function(result, alphabet) {
    var mul = BigInteger(1);
    var total = BigInteger(0);
    for (var k = result.length-1; k >= 0; k--) {
      // Work out this digits index in the alphabet
      var v = alphabet.indexOf(result.charAt(k));
      // Add it to the total, by the column multiple
      total = total.add(mul.multiply(v));
      // Multiply column multiple, by alphabet length
      mul = mul.multiply(alphabet.length);
    }
    return total;
  }

  var generateSalt = function(password, site) {

    var bignum = BigInteger(0);

    var result = hex_hmac_md5(password, site);

    var bignum = convertBase(result, "0123456789abcdef");

    var splitter = valueSplit(bignum);

    salt = "";
    for (var k = 0; k < 22; k++) {
      salt += splitter.choice(bcrypt_alphabet);
    }

    return salt;
  };

  exports.generatePassword = function(password, site, callback) {
    var letters     = 'abcdefghijklmnopqrstuvwxyz';
    var u_letters   = letters.toUpperCase();
    var all_letters = letters + u_letters;

    var numbers = '0123456789',
        symbols = '!@#$%*-?+=',
        length = 10;

    var salt = '$2a$11$' + generateSalt(password, site);

    var bcrypt = new bCrypt();
    bcrypt.hashpw(password, salt, function(result) {
      var hash = result.substring(salt.length);
      var bignum = convertBase(hash, bcrypt_alphabet);

      var splitter = valueSplit(bignum);

      // Create the final letters first
      var output = [];
      for (var n = 0; n < length-1; n++) {
        output.push(null);
      }

      // Pick atleast one of each group for the rest
      var groups = [letters, u_letters, numbers, symbols];

      for (var k=0; k < groups.length; k++) {
        // Find all remaining options
        var options = [];
        for (var j = 0; j < output.length; j++) {
          if (output[j] === null) options.push(j);
        }

        // Pick one of the remaining positions for this group
        var index = splitter.choice(options);

        // Insert a choice from the group in the position
        output[index] = splitter.choice(groups[k]);
      }

      // Fill in remaining blanks with any group
      for (var k = 0; k < output.length; k++) {
        if (output[k] === null) {
          output[k] = splitter.choice(groups.join(""))
        }
      }

      // Pick a letter for the front
      var front = splitter.choice(letters+u_letters);

      // ... and you have your password!
      callback(front + output.join(""));
    }, function() {});
  };
})(typeof exports !== 'undefined' ? exports : this);
