//
//  main.swift
//  Capston498
//
//  Created by Matt Hayden on 10/28/17.
//  Copyright © 2017 MSPA. All rights reserved.
//

import Foundation

let stderr = FileHandle.standardError
let stdout = FileHandle.standardOutput
var partNumberParentMap: [String:String] = [:]
let play = false

enum ActivityType: Int {
    case select = 1
    case detail
    case strong
    
    static func getType(fileType: Int) -> Int {
        if fileType == 2 {return ActivityType.select.rawValue}
        if fileType == 3 {return ActivityType.detail.rawValue}
        return ActivityType.strong.rawValue
        //        add_to_order    1   (adding an item to the order)
        //        part_number        2    (selecting a part number)
        //        product_detail    3    (selecting to see detail on a part number)
        //        print_product_detail    4 (printing the detail)
        //        save_cad        5    (saving a cad drawing from the detail)
    }
}
class Item {
    var partNumber: String = ""
    var parent: String = ""
    var specs: [(Int,Int)] = []
    
    init?(_ line: String) {
        let data = line.components(separatedBy: "\t")
        if data.count >= 5 {
            partNumber = data[0]
            parent = data[1]
            
            for attrIdx in stride(from: 4, to: data.count - 1, by: 2) {
                if let attrId = Int(data[attrIdx]), let valId = Int(data[attrIdx+1]) {
                    specs.append((attrId, valId))
                }
            }
        } else {
            return nil
        }
    }
}

class Activity {
    var visitorId: Int64 = -1
    var partNumber: String = ""
    var parent: String = ""
    var type: Int = -1
    var date: Int = -1
    var count: Int = -1
    var rating: Double?
    var countWeighted: Double?
    var ratingWeighted: Double?
    
    init() {}
    
    init?(_ line: String) {
        let data = line.components(separatedBy: ",")
        if data.count >= 5 {
            visitorId = Int64(data[0]) ?? -1
            partNumber = data[1]
            parent = partNumberParentMap[partNumber] ?? ""
            type = Int(data[2]) ?? -1
            date = Int(data[3]) ?? -1
            count = Int(data[4]) ?? -1
        } else {
            return nil
        }
    }
}

private func processActivityFile(_ filePath: String) throws -> [Activity] {
    log("processActivityFile start")
    var activityFile = try String(contentsOfFile: filePath, encoding: String.Encoding.utf8)
    let lines = activityFile.components(separatedBy: "\r\n")
    activityFile = ""
    var activities: [Activity] = []
    var counter = 1
    for line in lines[1..<lines.count] {
        if let act = Activity(line) {
            activities.append(act)
        }
        
        if counter % 500000 == 0 {
            print("processed \(counter) activities")
        }
        counter += 1
    }
    
    log("processActivityFile end")
    
    return activities
}

private func processItemFile(_ filePath: String) throws -> [Item] {
    log("processItemFile start")
    
    var itemsFile = try String(contentsOfFile: filePath, encoding: String.Encoding.utf8)
    let lines = itemsFile.components(separatedBy: "\r\n")
    itemsFile = ""
    var items: [Item] = []
    var counter = 1
    var count = lines.count
    if play {count = 50000}
    for line in lines[1..<count] {
        if let item = Item(line) {
            items.append(item)
        }
        if counter % 100000 == 0 {
            print("Processed \(counter) items")
        }
        counter += 1
    }
    log("processItemFile end")
    
    return items
}

private func itemToParentMap(_ items: [Item]) -> [String: String] {
    log("itemToParentMap start")
    
    var map: [String: String] = [:]
    for item in items {
        map[item.partNumber] = item.parent
    }
    
    log("itemToParentMap end")
    
    return map
}

private func sortActivitiesByVisitorDateParent(_ activities: inout [Activity]) {
    log("sortActivities start")
    
    activities.sort(by: { one, two in
        if one.visitorId == two.visitorId {
            if one.date == two.date {
                return one.parent > two.parent
            } else {
                return one.date < two.date
            }
        }
        return one.visitorId < two.visitorId
    })
    log("sortActivities end")
    
}

private func sortActivitiesByVisitorParent(_ activities: inout [Activity]) {
    log("sortActivities start")
    
    activities.sort(by: { one, two in
        if one.visitorId == two.visitorId {
            return one.parent < two.parent
        }
        return one.visitorId < two.visitorId
    })
    log("sortActivities end")
    
}

private func log(_ message: String) {
    let dateFormatter : DateFormatter = DateFormatter()
    dateFormatter.dateFormat = "HH:mm:ss"
    let date = Date()
    let dateString = dateFormatter.string(from: date)
    print("\(message) at \(dateString)")
}

private func massageForHighestParentActivityPerDay(_ activities: inout [Activity]) -> [Activity] {
    log("massageForHighestParentActivityPerDay start")
    
    sortActivitiesByVisitorDateParent(&activities)
    
    var filteredActivities: [Activity] = []
    var visitorId: Int64 = -1
    var parent: String = ""
    var date: Int = -1
    var maxActivity: Int = -1
    for activity in activities {
        if activity.visitorId == visitorId, activity.parent == parent, activity.date == date {
            let currType = ActivityType.getType(fileType: activity.type)
            if currType > maxActivity {
                maxActivity = currType
            }
        } else if visitorId != -1 {
            let filteredActivity = Activity()
            filteredActivity.count = 1
            filteredActivity.date = date
            filteredActivity.parent = parent
            filteredActivity.type = maxActivity
            filteredActivity.visitorId = visitorId
            filteredActivities.append(filteredActivity)
            
            visitorId = activity.visitorId
            parent = activity.parent
            date = activity.date
            maxActivity = ActivityType.getType(fileType: activity.type)
        } else {
            visitorId = activity.visitorId
            parent = activity.parent
            date = activity.date
            maxActivity = ActivityType.getType(fileType: activity.type)
        }
    }
    let filteredActivity = Activity()
    filteredActivity.count = 1
    filteredActivity.date = date
    filteredActivity.parent = parent
    filteredActivity.type = maxActivity
    filteredActivity.visitorId = visitorId
    filteredActivities.append(filteredActivity)
    
    log("massageForHighestParentActivityPerDay end")
    
    return filteredActivities
}

private func assignRatingsForActivityTypeWeight(activities: inout [Activity]) -> [Activity] {
    log("assignRatingsForActivityTypeWeight start")
    
    sortActivitiesByVisitorParent(&activities)
    
    var counter = 1
    let total = activities.count
    
    var ratedActivities: [Activity] = []
    var visitorId: Int64 = -1
    var parent: String = ""
    var anyCount: Int = -1
    var weightedCount: Double = -1
    for activity in activities {
        if activity.visitorId == visitorId, activity.parent == parent {
            // do nada
        } else if visitorId != -1 {
            let ratedActivity = Activity()
            ratedActivity.count = anyCount
            ratedActivity.countWeighted = weightedCount
            ratedActivity.rating =  round((2 * log(Double(anyCount + 1))) * 10)/10
            ratedActivity.ratingWeighted = round((2 * log(Double(weightedCount + 1))) * 10)/10
            ratedActivity.parent = parent
            ratedActivity.visitorId = visitorId
            ratedActivities.append(ratedActivity)
            
            visitorId = activity.visitorId
            parent = activity.parent
            anyCount = 0
            weightedCount = 0
        } else {
            visitorId = activity.visitorId
            parent = activity.parent
            anyCount = 0
            weightedCount = 0
        }
        
        anyCount += 1
        switch activity.type {
        case 1:
            weightedCount += 1
        case 2:
            weightedCount += 1.5
        case 3:
            weightedCount += 3
        default:
            break
        }
        
        if counter % 1000000 == 0 {
            print("processed \(counter) of \(total) activities")
        }
        counter += 1
    }
    let ratedActivity = Activity()
    ratedActivity.count = anyCount
    ratedActivity.countWeighted = weightedCount
    ratedActivity.rating =  round((2 * log(Double(anyCount + 1))) * 10)/10
    ratedActivity.ratingWeighted = round((2 * log(Double(weightedCount + 1))) * 10)/10
    ratedActivity.parent = parent
    ratedActivity.visitorId = visitorId
    ratedActivities.append(ratedActivity)
    
    log("assignRatingsForActivityTypeWeight start")
    
    return ratedActivities
}

private func writeActivitiesToFile(fileName: String, activities: [Activity]) {
    
    log("writeActivitiesToFile start")
    
    let dir = try? FileManager.default.url(for: .documentDirectory,
                                           in: .userDomainMask,
                                           appropriateFor: nil,
                                           create: true)
    
    // If the directory was found, we write a file to it and read it back
    if let fileURL = dir?.appendingPathComponent(fileName).appendingPathExtension("csv") {
        
        var output: [String] = ["VisitorId,ActionDate,Parent,ActionId,ActionCount\n"]
        activities.map({activity in
            return "\(activity.visitorId),\(activity.date),\(activity.parent),\(activity.type),\(activity.count)\n"
        }).forEach() {
            output.append($0)
        }
        
        let outString = output.joined()
        do {
            try outString.write(to: fileURL, atomically: true, encoding: .utf8)
        } catch {
            print("Failed writing to URL: \(fileURL), Error: " + error.localizedDescription)
        }
    }
    log("writeActivitiesToFile end")
}

private func writeRatedActivitiesToFile(fileName: String, activities: [Activity]) {
    
    log("writeRatedActivitiesToFile start")
    
    let dir = try? FileManager.default.url(for: .documentDirectory,
                                           in: .userDomainMask,
                                           appropriateFor: nil,
                                           create: true)
    
    // If the directory was found, we write a file to it and read it back
    if let fileURL = dir?.appendingPathComponent(fileName).appendingPathExtension("csv") {
        
        var output: [String] = ["VisitorId,Parent,AnyCount,AnyRating,WeightedCount,WeightedRating\n"]
        activities.map({activity in
            let isInteger = (activity.countWeighted ?? 0).truncatingRemainder(dividingBy: 1) == 0
            
            if isInteger {
                return "\(activity.visitorId),\(activity.parent),\(activity.count),\(activity.rating ?? 0),\(Int(activity.countWeighted ?? 0)),\(activity.ratingWeighted ?? -1)\n"
            } else {
                return "\(activity.visitorId),\(activity.parent),\(activity.count),\(activity.rating ?? 0),\(activity.countWeighted ?? 0),\(activity.ratingWeighted ?? -1)\n"
            }
        }).forEach() {
            output.append($0)
        }
        
        let outString = output.joined()
        do {
            try outString.write(to: fileURL, atomically: true, encoding: .utf8)
        } catch {
            print("Failed writing to URL: \(fileURL), Error: " + error.localizedDescription)
        }
    }
    log("writeRatedActivitiesToFile end")
}



do {
    let items = try processItemFile("/Users/haydude/Development/mspa/498 - Capstone/data/obfuscatedItems_10_17_17.txt")
    
    partNumberParentMap = itemToParentMap(items)
    
    var fileName = "obfuscatedWebActivity1024.csv"
    if play {fileName = "obfuscatedWebActivity1024play.csv"}
    var activities = try processActivityFile("/Users/haydude/Development/mspa/498 - Capstone/data/10-24 run/\(fileName)")
    
    
    var highestActivitiesPerDay = massageForHighestParentActivityPerDay(&activities)
    
    activities = []
    
    writeActivitiesToFile(fileName: "webActivityHighestPerDay", activities: highestActivitiesPerDay)
    
    //let ratedAnyActivities = assignRatingsForAnyActivity(activities: highestActivitiesPerDay)
    
    //writeRatedActivitiesToFile(fileName: "webActivityAnyRated", activities: ratedAnyActivities)
    
    let ratedWeightedActivities = assignRatingsForActivityTypeWeight(activities: &highestActivitiesPerDay)
    
    writeRatedActivitiesToFile(fileName: "webActivityRated", activities: ratedWeightedActivities)
    
    
} catch {
    print("Whoops! An error occurred: \(error)")
}
print("FINISHED")
