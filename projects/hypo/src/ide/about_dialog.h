#ifndef ABOUT_DIALOG_H_
#define ABOUT_DIALOG_H_

#include <QDialog>

QT_BEGIN_NAMESPACE
namespace Ui {
class AboutDialog;
}
QT_END_NAMESPACE

class AboutDialog : public QDialog {
  Q_OBJECT

 public:
  AboutDialog(QWidget* parent = nullptr);
  ~AboutDialog();

 private slots:
  void OnFinished();

 private:
  Ui::AboutDialog* ui;
};
#endif  // ABOUT_DIALOG_H_
