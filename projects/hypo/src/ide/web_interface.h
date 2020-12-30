#include <qobject.h>
#include <qwidget.h>

class WebInterface : public QObject {
  Q_OBJECT

 public:
  WebInterface(QWidget* parent = Q_NULLPTR);

  void ChangeData(const QString& data);

 public slots:
  void generateRandomData();

 signals:
  void dataChanged(const QString& data);
};
