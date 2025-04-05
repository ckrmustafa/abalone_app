# Abalone Yaş Tahmini Shiny Uygulaması

Bu proje, deniz salyangozlarının (abalone) fiziksel özelliklerine dayanarak yaşlarını tahmin etmek için geliştirilmiş bir makine öğrenimi uygulamasıdır. R Shiny ile oluşturulmuş interaktif bir arayüz sunar.

![Örnek Görsel](https://via.placeholder.com/600x400) <!-- Uygulama ekran görüntüsü ekleyin -->

## 📋 Özellikler
- 6+ makine öğrenimi modeli (Random Forest, XGBoost, SVM vb.)
- Veri analizi ve görselleştirme araçları
- Gerçek zamanlı tahmin yeteneği
- Model performans karşılaştırmaları

## 🛠️ Kurulum

### Gereksinimler
- R (v4.3.1 veya üzeri)
- RStudio (Tercihen)

### Adımlar
1. Gerekli R paketlerini yükleyin:
```r
install.packages(c("shiny", "caret", "randomForest", "xgboost", "dplyr", "ggplot2"))

Uygulamayı Github'dan çlaıştırın
shiny::runGitHub("abalone_app", "ckrmustafa")

Docker ile Çalıştırma
docker build -t abalone-app .
docker run -p 3838:3838 abalone-app

🖥️ Kullanım Kılavuzu
Veri Görünümü: Ham veriyi tablo halinde inceleyin

EDA: Korelasyon matrisi ve özellik önem grafikleri

Model Eğitimi:

İstediğiniz modelleri seçin

Cross-validation ve train-test split parametrelerini ayarlayın

Sonuçlar: Model performansını karşılaştırın

Tahmin: Yeni veri girişiyle anlık tahmin yapın

📂 Veri Kaynağı
Orijinal veri: UCI Machine Learning Repository

Temizlenmiş veri: data/abalone_clean.csv

🤝 Katkıda Bulunma
1-Repoyu forklayın

2-Yeni branch oluşturun:
git checkout -b yeni-ozellik

3-Değişiklikleri commit edin:
git commit -m "Yeni özellik eklendi"

4.Pull Request gönderin

MIT Lisansı - Detaylar için LICENSE dosyasına bakın.

İletişim: [Mustafa ÇAKIR] - [ckrmustafa@yahoo.com]
Güncelleme Tarihi: 05/04/2025





