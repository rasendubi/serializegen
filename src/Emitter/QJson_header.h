#include <QJsonValue>
#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>

template<typename T>
struct JsonSerializer {
    static QJsonValue toJsonValue(const T& value);

    static T fromJsonValue(QJsonValue);
};

template<typename T>
QJsonValue toJsonValue(const T& value) {
    return JsonSerializer<T>::toJson(value);
}

template<typename T>
T fromJsonValue(QJsonValue value) {
    return JsonSerializer<T>::fromJson(value);
}

template<>
struct JsonSerializer<double> {
    static QJsonValue toJson(const double& value) {
        return QJsonValue(value);
    }

    static double fromJson(QJsonValue value) {
        return value.toDouble();
    }
};

template<typename T>
QList<T> fromJsonList(QJsonArray array) {
    QList<T> result(array.size());
    for (const auto& value : array) {
        result.append(fromJsonValue<T>(value));
    }
    return result;
}

template<typename T>
QJsonArray toJsonList(QList<T> values) {
    QJsonArray result(values.size());
    for (const auto& value : values) {
        result.append(toJsonValue<T>(value));
    }
    return result;
}

