var REG_NONE = NewRegistrar('none', 'NONE')
var DNS_GCLOUD = NewDnsProvider("gcloud", "GCLOUD");
var DNS_BIND = NewDnsProvider('bind', 'BIND');  // ISC BIND.

D("example.com", REG_NONE, DnsProvider(DNS_BIND),
    A("test","1.2.3.4")
);

D("draines.com", REG_NONE, DnsProvider(DNS_GCLOUD),
  A("@","34.120.129.8", TTL(30)),
  A("www","34.120.129.8", TTL(30)),
  AAAA("@","2600:1901:0:1690::", TTL(30)),
  AAAA("www","2600:1901:0:1690::", TTL(30)),
  A("test","1.2.3.5"),
  AAAA("test","dead:beef::0")
);
