var REG_NAMECOM = NewRegistrar("name.com","NAMEDOTCOM");
var GCLOUD = NewDnsProvider("gcloud", "GCLOUD");

D("example.tld", REG_NAMECOM, DnsProvider(GCLOUD),
    A("test","1.2.3.4")
);
