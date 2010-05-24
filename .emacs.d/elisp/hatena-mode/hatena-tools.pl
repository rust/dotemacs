#!/usr/bin/perl -w

use strict;
use XMLRPC::Lite;
use Jcode;
use LWP::Simple;
use HTML::Parser;

my $rpc = XMLRPC::Lite->new;
$rpc->proxy('http://d.hatena.ne.jp/xmlrpc');

my $body = '*[emacs] hatena-mode

perlに変換演算子というのがあるのを知らなかった。

*1128958699*[math] WSネットワークと晩御飯ドラフト

-一人暮らしは、材料のやりくりが大変だから、"何々しよう"と作るよりも、基本の材料を取っておいて、そこから可能性のあるもの、すなわちネットワーク的に近いものが晩御飯になる。
-ここでいうネットワークとは、食材をノードとして"調理"することによって多次元ネットワークの間を移動するような、"夕飯きょうなにしようかな多層ネットワークモデル"である。
-晩御飯が"レシピ"ありきではなく、知識と冷蔵庫の中身が想起するものなら、ちょっと違ったモデル化が出来る。
-冷蔵庫のIT化、を「生活のIT化」と一般にとらえるなら、業務スーパーの情報を取りいれて買いものにいく主婦の視点とか、そういう視点でやるのがいい。目的は最適化で、そういう思考はわりと一般的だ。"iPodでpodcastingしてラジオから高音質な音楽情報を得る。"とか、"これからのサークル内の立場を考えて、新歓には出席する"とか、"金を郵便局に預けるよりも、投資して儲ける"とか、こうこうこうしたら生活がうまくいく、という"知恵"を出発点としていなければならない。
';

my $res = $rpc->call(
  'hatena.setKeywordLink',
  {
    body => XMLRPC::Data->type('string',Jcode->new($body,'euc')->utf8),
    score => 20,
    cname => ['book','movie'],
    a_target => '_blank',
    a_class => 'keyword',
  }
);
if (my $fault = $res->fault){
  for (keys %{$fault}){
    warn $_."=>".$fault->{$_};
  }
} else {
  $body = $res->result;
  $body =~ s/&lt;/</ig;
  $body =~ s/&gt;/>/ig;
  $body =~ s/&quote;/"/ig;
  $body = Jcode->new($body,'utf8')->euc;
}


my @member = ();

while($body =~ m/(http:\/\/d\.hatena\.ne\.jp[^">]+)/g){
@member = (@member, $&);
    }

#キーワードのページから、とってきて、ランダムに吐く
#$body から ランダムにURLを取ってきて、消す。
my $num = @member;
my $targeturl = get($member[int(rand $num)]);

my $num2 = 4;
my $p;
if($targeturl =~ m/<a href="(\/[a-zA-z0-9]+\/[0-9]+)">/){
    $p->parse( get('http://d.hatena.ne.jp'.$1) );
print $p;

}
