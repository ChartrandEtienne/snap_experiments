-- select url_prefix from comic where substring('http://www.egscomics.com/index.php?id=1' from (url_prefix || '%')) is not null;

-- select strpos('http://www.egscomics.com/index.php?id=11111', 'http://www.egscomics.com/index.php?id=');

-- select * from comic where strpos('http://www.egscomics.com/index.php?id=11111', url_prefix) != 0;

select * from visit where strpos(url, 'http://www.egscomics.com/index.php?id=') != 0;

