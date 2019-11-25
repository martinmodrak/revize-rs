<?php
if(isset($_GET["zdroj"])) {
	$zdroj = urlencode($_GET["zdroj"]);
	$zdroj_raw = "/?zdroj=".$zdroj;
} else {
	$zdroj = "neuvedeno_redirect";	
	$zdroj_raw = "";
}

if(isset($_GET["code"])) {
	$code_raw = "&code=".urlencode($_GET["code"]);
} else {
	$code_raw = "";
}

if( $zdroj == "fb_share") {
 $image = "https://roverskypruzkum.skaut.cz/img/hotovo.png?v=2";
} else { 
 $image = "https://roverskypruzkum.skaut.cz/img/hlavni.png?v=2";
}

?>
<html>
    <head>
<title>Roverský průzkum</title>
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta property="og:title" content="Roverský průzkum"/>
<meta property="og:image" content="<?php echo($image)?>"/>
<meta property="og:image:width" content="600" />
<meta property="og:image:height" content="600" />
<meta property="og:url" content="https://roverskypruzkum.skaut.cz<?php echo($zdroj_raw)?>"/>
<meta property="og:site_name" content="skaut.cz"/>
<meta property="og:type" content="website"/>
<meta property="og:description" content="Společně za lepší rovering. Končí 8. prosince 2019"/>

<meta property="fb:app_id" content="746547445843369"/>

<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="Roverský průzkum" />
<meta name="twitter:image" content="<?php echo($image)?>" />
<meta name="twitter:image:alt" content="Logo průzkumu" />
<meta name="twitter:url" content="https://roverskypruzkum.skaut.cz<?php echo($zdroj_raw)?>" />
<meta name="twitter:description" content="Společně za lepší rovering. Končí 8. prosince 2019" />

<link rel="icon" type="image/png" sizes="32x32" href="https://www.skaut.cz/wp-content/uploads/fbrfg/favicon-32x32.png">
<link rel="icon" type="image/png" sizes="16x16" href="https://www.skaut.cz/wp-content/uploads/fbrfg/favicon-16x16.png">
<style>
body {
	margin:0;
	padding:0;
	background-color:#84fc97;
	color:rgb(0, 65, 155);
	text-align:center;
}

iframe {
	height:100%;
	border-style:none;
	margin:0;
	padding:0;
	width: 1px;
    min-width: 100%;
    *width: 100%;	
}
</style>
</head>
<body>
<script>
window.location = "https://roverskypruzkum.formr.org/?zdroj=<?php echo($zdroj.$code_raw); ?>";
</script>
<h2>Spouštím průzkum...</h2>
<noscript>Roverský průzkum bohužel potřebuje zapnutý Javascript.</noscript>
</body>

